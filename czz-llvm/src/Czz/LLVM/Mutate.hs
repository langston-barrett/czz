{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.Mutate
  ( mutate
  )
where

import           Control.Monad (foldM)
import qualified Data.Maybe as Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified System.Random as Random

import qualified Czz.Log as Log
import           Czz.KLimited (IsKLimited)
import qualified Czz.Mutate.ByteString as MutBS
import qualified Czz.Mutate.Seq as MutSeq
import qualified Czz.Random as Rand
import           Czz.Record (Record)
import qualified Czz.Record as Rec
import           Czz.Seed (Seed)
import qualified Czz.Seed as Seed
import           Czz.SysTrace (type Time(Begin))

import qualified Czz.LLVM.CString as CStr
import           Czz.LLVM.Env (Env)
import qualified Czz.LLVM.Env as Env
import qualified Czz.LLVM.Env.Args as Args
import qualified Czz.LLVM.Env.FileSystem as FS
import           Czz.LLVM.Feedback (Feedback)
import qualified Czz.LLVM.Feedback as FB
import           Czz.LLVM.Overrides (Effect)

-- TODO(lb): mutation schedule
-- TODO(lb): mutate the trace
mutate ::
  Log.Has Text =>
  IsKLimited k =>
  Record Env Effect (Feedback k) ->
  IO (Seed 'Begin Env Effect)
mutate r = do
  let seed = Seed.rewind (Rec.seed r)
  mutated <- do
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
      [ mutArgv
      , mutEnvp
      , addEnvFromRead
      , newFileFromOpen
      , dropFile
      , randomizeFile
      ]

    mutCString range = fmap CStr.terminate . MutBS.any range . CStr.toByteString

    mutArgs f s = do
      let env = Seed.env s
      args <- Args.mutArgsA f (Env.args env)
      return env { Env.args = args }

    mutArgv =
      mutArgs $
        MutSeq.any
          (mutCString (0, 64))
          (CStr.terminate <$> MutBS.new (0, 64))
          (0, 32)

    mutEnvs f s = do
      let env = Seed.env s
      args <- Args.mutEnvsA f (Env.args env)
      return env { Env.args = args }

    mutEnvp =
      mutEnvs $
        MutSeq.any
          (mutCString (0, 64))
          (CStr.terminate <$> MutBS.new (0, 64))
          (0, 32)

    addEnvFromRead s = do
      let env = Seed.env s
      newEnv <- Rand.pickSet (FB.envVarsRead (Rec.feedback r))
      case newEnv of
        Nothing -> return env
        Just var -> do
          Log.debug ("Adding env var " <> Text.pack (show var))
          val <- MutBS.new (0, 64)  -- TODO(lb): size?
          return env { Env.args = Args.addWellFormedEnv var val (Env.args env) }

    -- TODO(lb): Lift out and use generic Map mutations

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
          Log.debug ("Adding file " <> Text.pack (show newPath))
          newFileContent <- Rand.genByteString (0, 256)  -- TODO(lb): size?
          return (FS.addFile newPath newFileContent envFs)

    dropFile s = mutFs s $ \envFs -> do
      let files = Vec.fromList (FS.paths envFs)
      path <- Rand.pickVec files
      case path of
        Nothing -> return envFs
        Just p -> do
          Log.debug ("Dropping file " <> Text.pack (show p))
          return (FS.rmFile p envFs)

    randomizeFile s = mutFs s $ \envFs -> do
      let files = Vec.fromList (FS.paths envFs)
      path <- Rand.pickVec files
      case path of
        Nothing -> return envFs
        Just p -> do
          newContent <- Rand.genByteString (0, 256)  -- TODO(lb): size?
          return (FS.addFile p newContent envFs)
