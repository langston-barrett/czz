{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Czz.SysTrace
  ( type Time(..)
  , SysTrace
  , SomeSysTrace(..)
  , begin
  , end
  , Step(..)
  , step
  , record
  , rewind
  , fastForward
  , snocBegin
  , snocEnd
  , unsnocBegin
  , unsnocEnd
  )
where

import           Prelude hiding (head, tail)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.Foldable as Fold
import qualified Data.Vector as Vec

import           Control.Category ((>>>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Time
  = Begin
  | Mid
  | End

-- | Trace of the effects of system calls
data SysTrace t e where
  TraceBegin ::
    -- | Events in the future
    Seq e ->
    SysTrace 'Begin e
  TraceMid ::
    -- | Events in the past
    Seq e ->
    -- | Events in the future
    Seq e ->
    SysTrace 'Mid e
  TraceEnd ::
    -- | Events in the past
    Seq e ->
    SysTrace 'End e

instance Eq e => Eq (SysTrace t e) where
  x == y =
    case (x, y) of
      (TraceBegin fut, TraceBegin fut') -> fut == fut'
      (TraceMid past fut, TraceMid past' fut') -> past == past' && fut == fut'
      (TraceEnd past, TraceEnd past') -> past == past'

instance Ord e => Ord (SysTrace t e) where
  compare x y =
    case (x, y) of
      (TraceBegin fut, TraceBegin fut') -> compare fut fut'
      (TraceEnd past, TraceEnd past') -> compare past past'
      (TraceMid past fut, TraceMid past' fut') ->
        if past /= past'
        then compare past past'
        else compare fut fut'

begin :: SysTrace 'Begin e
begin = TraceBegin Seq.empty

end :: SysTrace 'End e
end = TraceEnd Seq.empty

-- | Defines the valid state transitions.
--
-- +-----------+-------------+--------+
-- | Old state | New state   | Output |
-- +===========+=============+--------+
-- | 'Begin'   | 'Mid'       | @e@    |
-- | 'Begin'   | 'End'       | none   |
-- | 'Mid'     | 'Mid'       | @e@    |
-- | 'Mid'     | 'End'       | none   |
-- | 'End'     | 'End'       | none   |
-- +-----------+-------------+--------+
data Step' t e where
  -- | The trace was empty
  StepBeginEnd :: SysTrace 'End e -> Step' 'Begin e
  -- | This is the first item in the trace
  StepBeginMid :: SysTrace 'Mid e -> e -> Step' 'Begin e
  -- | There are more items in the trace
  StepMidMid :: SysTrace 'Mid e -> e -> Step' 'Mid e
  -- | This is the last item in the trace
  StepMidEnd :: SysTrace 'End e -> Step' 'Mid e
  -- | The trace is already consumed
  StepEndEnd :: SysTrace 'End e -> Step' 'End e

-- | Observe the current state, and step one forward.
--
-- /O(1)/.
step' :: SysTrace t e -> Step' t e
step' =
  \case
    TraceBegin Seq.Empty ->
      StepBeginEnd end
    TraceBegin (pres Seq.:<| fut) ->
      StepBeginMid (TraceMid (Seq.singleton pres) fut) pres
    TraceMid past (pres Seq.:<| fut) ->
      StepMidMid (TraceMid (past Seq.:|> pres) fut) pres
    TraceMid past Seq.Empty ->
      StepMidEnd (TraceEnd past)
    t@TraceEnd{} ->
      StepEndEnd t

data Step e where
  StepMid :: SysTrace 'Mid e -> e -> Step e
  StepEnd :: SysTrace 'End e -> Step e

-- | Observe the current state, and step one forward.
--
-- /O(1)/.
step :: SysTrace t e -> Step e
step =
  step' >>>
    \case
      StepBeginEnd t -> StepEnd t
      StepBeginMid t e -> StepMid t e
      StepMidMid t e -> StepMid t e
      StepMidEnd t -> StepEnd t
      StepEndEnd t -> StepEnd t

-- | Move forward in time, without observation.
--
-- /O(1)/.
_tick :: SysTrace t e -> SomeSysTrace e
_tick = stepTrace . step'
  where
    stepTrace :: Step' t e -> SomeSysTrace e
    stepTrace =
      \case
        StepBeginEnd t -> SomeSysTrace t
        StepBeginMid t _ -> SomeSysTrace t
        StepMidMid t _ -> SomeSysTrace t
        StepMidEnd t -> SomeSysTrace t
        StepEndEnd t -> SomeSysTrace t

data SomeSysTrace e = forall t. SomeSysTrace (SysTrace t e)

-- | Record the past.
record :: e -> SysTrace 'End e -> SysTrace 'End e
record e (TraceEnd past) = TraceEnd (past Seq.|> e)

-- | Rewind time so that the past becomes the future
--
-- /O(log(n))/.
rewind :: SysTrace t e -> SysTrace 'Begin e
rewind =
  \case
    t@TraceBegin{} -> t
    TraceMid past fut -> TraceBegin (past Seq.>< fut)
    TraceEnd past -> TraceBegin past

-- | Fast-forward so that the future becomes the past
--
-- /O(log(n))/.
fastForward :: SysTrace t e -> SysTrace 'End e
fastForward =
  \case
    TraceBegin fut -> TraceEnd fut
    TraceMid past fut -> TraceEnd (past Seq.>< fut)
    t@TraceEnd{} -> t

snocBegin :: SysTrace 'Begin e -> e -> SysTrace 'Begin e
snocBegin t e =
  case t of
    TraceBegin fut -> TraceBegin (fut Seq.|> e)

snocEnd :: SysTrace 'End e -> e -> SysTrace 'End e
snocEnd t e =
  case t of
    TraceEnd past -> TraceEnd (past Seq.|> e)

unsnocBegin :: SysTrace 'Begin e -> Maybe (SysTrace 'Begin e, e)
unsnocBegin =
  \case
    TraceBegin (fut Seq.:|> e) -> Just (TraceBegin fut, e)
    TraceBegin Seq.Empty -> Nothing

unsnocEnd :: SysTrace 'End e -> Maybe (SysTrace 'End e, e)
unsnocEnd =
  \case
    TraceEnd (past Seq.:|> e) -> Just (TraceEnd past, e)
    TraceEnd Seq.Empty -> Nothing

--------------------------------------------------------------------------------
-- JSON

instance Aeson.ToJSON e => Aeson.ToJSON (SysTrace 'End e) where
  toJSON =
    Aeson.Object .
      \case
        TraceEnd past -> AesonKM.singleton "past" (toArray past)
    where toArray s = Aeson.Array (fmap Aeson.toJSON (Vec.fromList (Fold.toList s)))

  toEncoding =
    \case
      TraceEnd past ->
        Aeson.pairs ("past" Aeson..= Fold.toList past)

instance Aeson.FromJSON e => Aeson.FromJSON (SysTrace 'End e) where
  parseJSON =
    Aeson.withObject "SysTrace" $ \v -> TraceEnd <$> v Aeson..: "past"
