{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.Mutate
  ( mutate
  )
where

import           Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified System.Random as Random

import qualified Czz.Log as Log
import           Czz.KLimited (IsKLimited)
import qualified Czz.Random as Rand
import           Czz.Record (Record)
import qualified Czz.Record as Rec
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed
import           Czz.SysTrace (type Time(Begin, End))

import           Czz.LLVM.Env (Env)
import qualified Czz.LLVM.Env as Env
import qualified Czz.LLVM.Env.Args as Args
import qualified Czz.LLVM.Env.FileSystem as FS
import           Czz.LLVM.Feedback (Feedback)
import qualified Czz.LLVM.Feedback as FB
import           Czz.LLVM.Overrides (Effect)
import qualified Czz.LLVM.Overrides as Ov
import qualified Czz.LLVM.Overrides.Libc as Libc
import qualified Czz.LLVM.Overrides.Posix as Posix

-- TODO(lb): mutation schedule
-- TODO(lb): mutate the trace
mutate ::
  Log.Has Text =>
  IsKLimited k =>
  Record Env Effect (Feedback k) ->
  IO (Seed 'Begin Env Effect)
mutate r = do
  let seed = Seed.rewind (Rec.seed r)
  mutTrace <- Random.randomIO :: IO Bool
  mutated <-
    if mutTrace
      then do
        Seed.mutLast mutEffect (Rec.seed r)
      else do
        numMuts <- Random.randomRIO (0, 8) :: IO Int
        let go state _n = do
              mut <- Rand.pickVec mutations
              mutated <- Maybe.fromJust mut state
              return (Seed.begin mutated)
        foldM go seed [0..numMuts]
  if mutated == seed
    then mutate r
    else return mutated
  where
    -- TODO(lb): generic bytestring mutations, applied to each stdin, file, arg,
    -- etc., including replace with constant from module. Also, start new files
    -- empty.

    mutations :: Vec.Vector (Seed 'Begin Env Effect -> IO Env)
    mutations =
      Vec.fromList
      [ addArg
      , dropArg
      , addEnv
      , dropEnv
      , addEnvFromRead
      , newFileFromOpen
      , dropFile
      , randomizeFile
      ]

    argvIdx as = Random.randomRIO (0, Args.argvLength as - 1)
    envpIdx as = Random.randomRIO (0, Args.envpLength as - 1)

    mutArgs ::
      Seed 'Begin Env Effect ->
      (Args.Template -> IO Args.Template) ->
      IO Env
    mutArgs s f = do
      let env = Seed.env s
      args' <- f (Env.args env)
      return env { Env.args = args' }

    addArg s = mutArgs s $ \envArgs -> do
      Log.log ?logger "Adding command-line argument..."
      idx <- argvIdx envArgs
      newArg <- Rand.genCString (0, 64)  -- TODO(lb): size?
      return (Args.addArg idx newArg envArgs)

    dropArg s = mutArgs s $ \envArgs -> do
      Log.log ?logger "Dropping command-line argument..."
      idx <- argvIdx envArgs
      return (Args.rmArg idx envArgs)

    addEnv s = mutArgs s $ \envArgs -> do
      Log.log ?logger "Adding env var..."
      idx <- envpIdx envArgs
      newEnv <- Rand.genCString (0, 64)  -- TODO(lb): size?
      return (Args.addEnv idx newEnv envArgs)

    dropEnv s = mutArgs s $ \envArgs -> do
      Log.log ?logger "Dropping env var..."
      idx <- envpIdx envArgs
      return (Args.rmEnv idx envArgs)

    addEnvFromRead s = mutArgs s $ \envArgs -> do
      newEnv <- Rand.pickSet (FB.envVarsRead (Rec.feedback r))
      Log.log ?logger ("Trying to add env " <> Text.pack (show (FB.envVarsRead (Rec.feedback r))))
      case newEnv of
        Nothing -> return envArgs
        Just var -> do
          Log.log ?logger ("Adding env var " <> Text.pack (show var))
          val <- Rand.genByteString (0, 64)  -- TODO(lb): size?
          return (Args.addWellFormedEnv var val envArgs)

    mutFs ::
      Seed 'Begin Env Effect ->
      (FS.Template -> IO FS.Template) ->
      IO Env
    mutFs s f = do
      let env = Seed.env s
      fs' <- f (Env.fs env)
      return $ env { Env.fs = fs' }

    newFileFromOpen s = mutFs s $ \envFs -> do
      newFilePath <- Rand.pickSet (FB.filesOpened (Rec.feedback r))
      case newFilePath of
        Nothing -> return envFs
        Just newPath -> do
          Log.log ?logger ("Adding file " <> Text.pack (show newPath))
          newFileContent <- Rand.genByteString (0, 256)  -- TODO(lb): size?
          return (FS.addFile newPath newFileContent envFs)

    dropFile s = mutFs s $ \envFs -> do
      let files = Vec.fromList (FS.paths envFs)
      path <- Rand.pickVec files
      case path of
        Nothing -> return envFs
        Just p -> do
          Log.log ?logger ("Dropping file " <> Text.pack (show p))
          return (FS.rmFile p envFs)

    randomizeFile s = mutFs s $ \envFs -> do
      let files = Vec.fromList (FS.paths envFs)
      path <- Rand.pickVec files
      case path of
        Nothing -> return envFs
        Just p -> do
          newContent <- Rand.genByteString (0, 256)  -- TODO(lb): size?
          return (FS.addFile p newContent envFs)

    _mutTrace ::
      Seed 'End Env Effect ->
      (Effect -> IO Effect) ->
      IO (Seed 'Begin Env Effect)
    _mutTrace s f = Seed.mutLast f s  -- TODO(lb): unsnoc variable number of times

    -- TODO(lb): mutation should hapen in situ - but how?
    mutEffect :: Effect -> IO Effect
    mutEffect =
      \case
        e@(Ov.Libc (Libc.Fprintf Libc.FprintfSuccess)) -> return e
        e@(Ov.Libc (Libc.Strcpy Libc.StrcpyEffect)) -> return e
        e@(Ov.Posix (Posix.Accept Posix.AcceptSuccess)) -> return e
        e@(Ov.Posix (Posix.Bind Posix.BindSuccess)) -> return e
        e@(Ov.Posix (Posix.Listen Posix.ListenSuccess)) -> return e
        Ov.Posix (Posix.Recv (Posix.RecvSuccess bs)) -> do
          Ov.Posix . Posix.Recv . Posix.RecvSuccess <$>
            Rand.genByteString (0, BS.length bs)
        Ov.Posix (Posix.Socket (Posix.SocketSuccess _i)) -> do
          Ov.Posix . Posix.Socket . Posix.SocketSuccess <$>
            Random.randomRIO (0, 64)
        e -> return e
