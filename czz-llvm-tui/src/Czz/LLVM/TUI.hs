{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.TUI
  (main)
where

import qualified Control.Concurrent as Con
import           Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as MState
import qualified Data.Foldable as Fold
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (NominalDiffTime, TimeZone)
import qualified Data.Time.Format as TimeF
import qualified Data.Time.LocalTime as TimeL
import qualified Data.Sequence as Seq
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
import qualified Brick.Widgets.Table as BWT
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as VtyE

import qualified Czz.Config.Type as CConf
import qualified Czz.Count as Count
import qualified Czz.Coverage.Seed as CSeed
import qualified Czz.Freq as Freq
import qualified Czz.Fuzz as Fuzz
import qualified Czz.Log as Log
import           Czz.Now (Now)
import qualified Czz.Now as Now
import qualified Czz.KLimited as KLimit
import qualified Czz.Record as Rec
import           Czz.State (State)
import qualified Czz.State as State
import qualified Czz.State.Stats as Stats
import qualified Czz.Stop as Stop

import qualified Czz.LLVM.Fuzz as CL
import           Czz.LLVM.Feedback (Feedback)
import qualified Czz.LLVM.Init as Init
import qualified Czz.LLVM.Translate as Trans

import qualified Czz.LLVM.TUI.Config.Type as Conf
import qualified Czz.LLVM.TUI.Config.CLI as CLI

data Event env eff k fb
  = FinalState (State env eff k fb)
  | NewState (State env eff k fb)

data TState env eff k fb
  = TState
    { stateNow :: Now
    , timeZone :: TimeZone
    , state :: State env eff k fb
    }

data TStates env eff k fb
  = InitState (TState env eff k fb)
  | HelpState Bool (TState env eff k fb)
  | NormalState Bool (TState env eff k fb)

padded :: Int -> Int -> [(Text, Text)] -> B.Widget ()
padded rpad lpad = B.vBox . map (uncurry row)
  where
    row l r =
      B.padRight (BW.Pad (rpad - BW.textWidth l)) (BW.txt l) B.<+>
      B.padLeft (BW.Pad (lpad - BW.textWidth r)) (BW.txt r)

topStats :: Bool -> TState env eff k fb -> B.Widget ()
topStats final tstate =
  let stats = State.stats (state tstate)
      now = stateNow tstate
  in padded
        10
        30
        [ ("start:", localTime tstate (Stats.start (State.stats (state tstate))))
        , if final
          then ("end:", localTime tstate (Now.getNow (stateNow tstate)))
          else ("now:", localTime tstate (Now.getNow (stateNow tstate)))
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

    localTime ts t =
      Text.pack $
        TimeF.formatTime TimeF.defaultTimeLocale "%F %T" $
          TimeL.utcToLocalTime (timeZone ts) t

llvmStats :: TState env eff k Feedback -> B.Widget ()
llvmStats tstate =
  BW.txt "stuck:"
  BW.<=>
  padded 30 10 (Fold.toList topLastLocs)
  where
    lastLocs :: [Text]
    lastLocs =
      Fold.toList $ CSeed.lastLoc . Rec.coverage <$>
        State.pool (state tstate)
    topLastLocs :: Seq.Seq (Text, Text)
    topLastLocs =
      (\(loc, ct) -> (loc <> ":", Text.pack (show (Count.toWord ct)))) <$>
        Seq.take 3 (Seq.reverse (Freq.sorted (Freq.count lastLocs)))

helpNumbers :: B.Widget ()
helpNumbers = B.vBox (map BW.txt expls)
  where
    expls =
      [ "start: time when czz was started"
      , "now: current time (end: when fuzzing stopped)"
      , "duration: difference between now and start"
      , "execs: total number of executions of target"
      , "execs/sec: executions per second"
      , "last new: time since last new coverage"
      , "pool: number of seeds in seed pool"
      , "missing: library functions with no model"
      ]

helpKbd :: B.Widget()
helpKbd = B.vBox (map BW.txt kbds)
  where
    kbds =
      [ "ESC: exit"
      , "up: toggle help"  -- TODO(lb): SPC? h? ??
      ]

-- TODO(lb): include url to docs
drawHelp :: B.Widget ()
drawHelp = BWT.renderTable (BWT.table [[helpNumbers], [helpKbd]])

draw :: TStates env eff k Feedback -> B.Widget ()
draw tstates =
  BWC.center $
    BWB.borderWithLabel (BW.txt "czz") $
      case tstates of
        InitState _tstate -> BW.txt "Starting..."
        HelpState _final _tstate -> drawHelp
        NormalState final tstate ->
          BWT.renderTable $
            BWT.table
              [ [topStats final tstate]
              , [llvmStats tstate]
              -- TODO(lb): center, SPC/h/?
              , [BW.txt "Press UP for help"]
              ]

app ::
  TimeZone ->
  App (TStates env eff k Feedback) (Event env eff k Feedback) ()
app tz =
  B.App
  { B.appDraw = \tstates -> [draw tstates]
  , B.appChooseCursor = B.neverShowCursor
  , B.appHandleEvent =
      \case
        B.VtyEvent (VtyE.EvKey VtyE.KEsc _) -> BMain.halt
        B.VtyEvent (VtyE.EvKey VtyE.KUp _) -> do
          now <- liftIO Now.now
          tstates <- MState.get
          case tstates of
            InitState{} -> return ()
            HelpState final tstate ->
              MState.put (NormalState final (tstate { stateNow = now }))
            NormalState final tstate ->
              MState.put (HelpState final (tstate { stateNow = now }))
        B.AppEvent (NewState st) -> newState False st
        B.AppEvent (FinalState st) -> newState True st
        _ -> return ()
  , B.appStartEvent = return ()
  , B.appAttrMap =
      const $ BAM.attrMap Vty.defAttr []
  }
  where
    newState isFinal st = do
      now <- liftIO Now.now
      tstates <- MState.get
      case tstates of
        InitState{} ->
          MState.put (NormalState False (TState now tz st))
        NormalState final _ ->
          MState.put (NormalState (final || isFinal) (TState now tz st))
        HelpState final _ ->
          MState.put (HelpState (final || isFinal) (TState now tz st))

main :: IO ExitCode
main = do
  stop <- Stop.new
  conf <- CLI.config
  let llvmConf = Conf.llvm conf
  let fuzzConf = Conf.fuzz conf
  translation <- Trans.translate llvmConf  -- Allowed to fail/call exit
  KLimit.withSomeKLimit (CConf.pathLen fuzzConf) $ do

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

    let simLog = Log.with Log.void Init.logToTempFile
    _threadId <- flip Con.forkFinally fuzzerDone $ do
        let fuzzer =
              (CL.llvmFuzzer llvmConf translation simLog Init.noExtraInit)
              { Fuzz.onUpdate = \tstates -> do
                  _didUpdate <-
                    BChan.writeBChanNonBlocking eventChan (NewState tstates)
                  return ()
              }
        Fuzz.fuzz fuzzConf stop fuzzer Log.void Log.void

    let buildVty = Vty.mkVty Vty.defaultConfig
    initialVty <- buildVty
    now <- Now.now
    tz <- TimeL.getCurrentTimeZone
    initState <- InitState . TState now tz <$> State.newIO
    _finalState <-
      B.customMain initialVty buildVty (Just eventChan) (app tz) initState

    return Exit.ExitSuccess
