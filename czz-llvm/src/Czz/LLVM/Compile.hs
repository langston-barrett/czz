{-# LANGUAGE LambdaCase #-}

module Czz.LLVM.Compile
  ( CommandFailure(..)
  , linkMusl
  , compileCFile
  , compileCFiles
  , compileAndLinkCFile
  , compileAndLinkCFiles
  )
where

import qualified Data.List as List
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import           System.FilePath ((</>))
import qualified System.Process as Proc

import qualified Czz.LLVM.Config.Type as Conf

import           Paths_czz_llvm (getDataDir)

data CommandFailure
  = CommandFailure
      { command :: !String
      , code :: !Int
      , sout :: String
      , serr :: String
      }

runProc :: String -> [String] -> IO (Either CommandFailure ())
runProc bin params = do
  (res, sout', serr') <- Proc.readProcessWithExitCode bin params ""
  return $
    case res of
      Exit.ExitSuccess -> Right ()
      Exit.ExitFailure code' ->
        Left $
          CommandFailure
            { command = bin ++ unwords params
            , code = code'
            , sout = sout'
            , serr = serr'
            }

doLinkMusl ::
  [String] ->
  FilePath ->
  IO (Either CommandFailure FilePath)
doLinkMusl opts prog = do
  dataDir <- getDataDir
  let muslDir = dataDir </> "musl"
  muslBcFiles <- filter (".bc" `List.isSuffixOf`) <$> Dir.listDirectory muslDir
  let outPath = "out.bc"
  let params =
        concat
          [ [prog]
          , map (muslDir </>) muslBcFiles
          , opts
          , ["-o", outPath]
          ]
  fmap (const outPath) <$> runProc "llvm-link" params

linkMusl :: Conf.Config -> IO (Either CommandFailure FilePath)
linkMusl conf =
  doLinkMusl
    ["--only-needed" | Conf.onlyNeeded conf]
    (Conf.prog conf)

compileCFile :: FilePath -> IO (Either CommandFailure FilePath)
compileCFile cFile = do
  let outPath = replaceSuf ".c" ".bc" cFile
  let params =
        concat
          [ [cFile]
          , ["-fno-discard-value-names", "-O1", "-emit-llvm", "-c"]
          , ["-Wall", "-Werror"]
          , ["-o", outPath]
          ]
  fmap (const outPath) <$> runProc "clang" params
  where
    replaceSuf suf suf' =
      reverse . (reverse suf' ++) . drop (length suf) . reverse

compileAndLinkCFile ::
  [String] ->
  FilePath ->
  IO (Either CommandFailure FilePath)
compileAndLinkCFile linkOpts cFile = do
  compileCFile cFile >>=
    \case
      Left e -> return (Left e)
      Right p -> doLinkMusl linkOpts p

getCFiles :: FilePath -> IO [FilePath]
getCFiles dir =
  map (dir </>) . filter (".c" `List.isSuffixOf`) <$>
    Dir.listDirectory dir
 
compileCFiles :: FilePath -> IO [Either CommandFailure FilePath]
compileCFiles dir = traverse compileCFile =<< getCFiles dir
 
compileAndLinkCFiles ::
  [String] ->
  FilePath ->
  IO [Either CommandFailure FilePath]
compileAndLinkCFiles linkOpts dir = 
  traverse (compileAndLinkCFile linkOpts) =<< getCFiles dir
