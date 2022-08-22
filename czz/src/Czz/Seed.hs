{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Czz.Seed
  ( Seed
  , env
  , effects
  , begin
  , end
  , record
  , rewind
  , snoc
  , unsnoc
  , mutLast
  )
where

import           Czz.SysTrace (SysTrace, Time(Begin, End))
import qualified Czz.SysTrace as SysTrace

data Seed t env eff
  = Seed
    { _env :: env
    , _effects :: SysTrace t eff
    }
  deriving (Eq, Ord)

env :: Seed t env eff -> env
env = _env

effects :: Seed t env eff -> SysTrace t eff
effects = _effects

begin :: env -> Seed 'Begin env eff
begin e =
  Seed
  { _env = e
  , _effects = SysTrace.begin
  }

end :: env -> Seed 'End env eff
end e =
  Seed
  { _env = e
  , _effects = SysTrace.end
  }

record :: Seed 'Begin env eff -> SysTrace 'End eff -> Seed 'End env eff
record s t = s { _effects = t }

rewind :: Seed t env eff -> Seed 'Begin env eff
rewind s = s { _effects = SysTrace.rewind (effects s) }

snoc :: Seed 'End env eff -> eff -> Seed 'Begin env eff
snoc s e = s { _effects = SysTrace.rewind (SysTrace.snocEnd (effects s) e) }

unsnoc :: Seed 'End env eff -> Maybe (Seed 'End env eff, eff)
unsnoc s =
  case SysTrace.unsnocEnd (effects s) of
    Just (effs, eff) -> Just (s { _effects = effs }, eff)
    Nothing -> Nothing

mutLast ::
  Applicative f =>
  (eff -> f eff) ->
  Seed 'End env eff ->
  f (Seed 'Begin env eff)
mutLast f s =
  case unsnoc s of
    Nothing -> pure (rewind s)
    Just (s', e) -> snoc s' <$> f e
