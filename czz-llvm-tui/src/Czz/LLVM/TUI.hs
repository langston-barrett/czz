{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.TUI
  (main)
where

import qualified Control.Concurrent as Con
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as MState
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (NominalDiffTime, TimeZone)
import qualified Data.Time.Format as TimeF
import qualified Data.Time.LocalTime as TimeL
import           System.Exit (ExitCode)
import qualified System.Exit as Exit
import           System.Time.Utils (renderSecs)

import           Brick (App)
import qualified Brick as B
import qualified Brick.AttrMap as BAM
import qualified Brick.BChan as BChan
import qualified Brick.Main as BMain
import qualified Brick.Widgets.Border as BWB
import qualified Brick.Widgets.Core as BW
import qualified Brick.Widgets.Center as BWC
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as VtyE

import qualified Czz.Config.Type as CConf
import qualified Czz.Fuzz as Fuzz
import qualified Czz.Log as Log
import           Czz.Now (Now)
import qualified Czz.Now as Now
import qualified Czz.KLimited as KLimit
import           Czz.State (State)
import qualified Czz.State as State
import qualified Czz.State.Stats as Stats
import qualified Czz.Stop as Stop

import qualified Czz.LLVM as CL
import qualified Czz.LLVM.Config.CLI as CLI
import qualified Czz.LLVM.Config.Type as Conf
import qualified Czz.LLVM.Translate as Trans

data Event env eff fb
  = FinalState (State env eff fb)
  | NewState (State env eff fb)

data TState env eff fb
  = TState
    { stateNow :: Now
    , timeZone :: TimeZone
    , state :: State env eff fb
    }

data TStates env eff fb
  = InitState (TState env eff fb)
  | OtherState (TState env eff fb)

padded :: Int -> Int -> [(Text, Text)] -> B.Widget ()
padded rpad lpad = B.vBox . map (uncurry row)
  where
    row l r =
      B.padRight (BW.Pad (rpad - BW.textWidth l)) (BW.txt l) B.<+>
      B.padLeft (BW.Pad (lpad - BW.textWidth r)) (BW.txt r)

draw :: TStates env eff fb -> B.Widget ()
draw tstates =
  BWC.center $
    BWB.borderWithLabel (BW.txt "czz") $
      case tstates of
        InitState _tstate -> BW.txt "Starting..."
        OtherState tstate ->
          let stats = State.stats (state tstate)
              now = stateNow tstate
          in padded
               10
               30
               [ ("start:", localTime tstate (Stats.start (State.stats (state tstate))))
               , ("now:", localTime tstate (Now.getNow (stateNow tstate)))
               , ("duration:", showTime (Stats.sinceStart stats now))
               , ("execs:", Text.pack (show (Stats.execs stats)))
               , ("execs/sec:", Text.pack (show (Stats.execsPerSec stats now)))
               , ("last new:", showTime (Stats.sinceLastNew stats now))
               , ("pool:", Text.pack (show (Stats.poolSize stats)))
               , ("missing:", Text.unlines (Set.toList (Stats.missing stats)))
               ]
  where
    showTime :: NominalDiffTime -> Text
    showTime = Text.pack . renderSecs . round

    localTime tstate t =
      Text.pack $
        TimeF.formatTime TimeF.defaultTimeLocale "%F %T" $
          TimeL.utcToLocalTime (timeZone tstate) t

app :: TimeZone -> App (TStates env eff fb) (Event env eff fb) ()
app tz =
  B.App
  { B.appDraw = \tstates -> [draw tstates]
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent =
      \case
        B.VtyEvent (VtyE.EvKey VtyE.KEsc _) -> BMain.halt
        B.AppEvent (NewState st) -> do
          now <- liftIO Now.now
          MState.put (OtherState (TState now tz st))
        _ -> return ()
  , B.appStartEvent = return ()
  , B.appAttrMap =
      const $ BAM.attrMap Vty.defAttr []
  }

main :: IO ExitCode
main = do
  stop <- Stop.new
  conf <- CLI.cliConfig
  translation <- Trans.translate conf  -- Allowed to fail/call exit
  KLimit.withKLimit (CConf.pathLen (Conf.common conf)) $ do

    eventChan <- BChan.newBChan 1

    let fuzzerDone =
          \case
            Left threadExc -> do
              print threadExc
              Exit.exitFailure
            Right (Left fuzzExc) -> do
              print fuzzExc
              Exit.exitFailure
            Right (Right final) -> BChan.writeBChan eventChan (FinalState final)

    _threadId <- flip Con.forkFinally fuzzerDone $ do
        let fuzzer =
              (CL.llvmFuzzer conf translation)
              { Fuzz.onUpdate = \tstates -> do
                  _didUpdate <-
                    BChan.writeBChanNonBlocking eventChan (NewState tstates)
                  return ()
              }
        Fuzz.fuzz (Conf.common conf) stop fuzzer Log.void Log.void

    let buildVty = Vty.mkVty Vty.defaultConfig
    initialVty <- buildVty
    now <- Now.now
    tz <- TimeL.getCurrentTimeZone
    initState <- InitState . TState now tz <$> State.newIO
    _finalState <-
      B.customMain initialVty buildVty (Just eventChan) (app tz) initState

    return Exit.ExitSuccess
