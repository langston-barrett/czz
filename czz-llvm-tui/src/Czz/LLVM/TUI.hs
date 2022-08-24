{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.TUI
  (main)
where

import qualified Control.Concurrent as Con
import           System.Exit (ExitCode)
import qualified System.Exit as Exit

import           Brick (App)
import qualified Brick as B
import qualified Brick.AttrMap as BAM
import qualified Brick.BChan as BChan
import qualified Brick.Main as BMain
import qualified Brick.Widgets.Core as BW
import qualified Brick.Widgets.Center as BWC
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as VtyE

import qualified Czz.Config.Type as CConf
import qualified Czz.Fuzz as Fuzz
import qualified Czz.Log as Log
import qualified Czz.KLimited as KLimit
import           Czz.State (State)
import qualified Czz.State as State

import qualified Czz.LLVM as CL
import qualified Czz.LLVM.Config.CLI as CLI
import qualified Czz.LLVM.Config.Type as Conf
import qualified Czz.LLVM.Translate as Trans

data Event env eff fb
  = FinalState (State env eff fb)
  -- | NewState (State env eff fb)

app :: App (State env eff fb) (Event env eff fb) ()
app =
  B.App
  { B.appDraw =
      \_state -> [BWC.center (BW.txt "czz")]
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent =
      \case
        B.VtyEvent (VtyE.EvKey VtyE.KEsc _) -> BMain.halt
        _ -> return ()
  , B.appStartEvent = return ()
  , B.appAttrMap =
      const $ BAM.attrMap Vty.defAttr []
  }

main :: IO ExitCode
main = do
  conf <- CLI.cliConfig
  translation <- Trans.translate conf  -- Allowed to fail/call exit
  KLimit.withKLimit (CConf.pathLen (Conf.common conf)) $ do

    eventChan <- BChan.newBChan 1

    -- TODO(lb): 
    _threadId <- flip Con.forkFinally (error . show) $ do
        let fuzzer = CL.llvmFuzzer conf translation
        Fuzz.fuzz (Conf.common conf) fuzzer Log.void Log.void >>=
          \case
            Left err -> do
              print err
              Exit.exitFailure
            Right finalState -> do
              BChan.writeBChan eventChan (FinalState finalState)
              return ()

    let buildVty = Vty.mkVty Vty.defaultConfig
    initialVty <- buildVty
    initState <- State.newIO
    _finalState <-
      B.customMain initialVty buildVty (Just eventChan) app initState

    return Exit.ExitSuccess
