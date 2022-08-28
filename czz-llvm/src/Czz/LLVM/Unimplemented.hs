{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Czz.LLVM.Unimplemented
  ( Unimplemented (..),
    ppUnimplemented,
    throw,
  )
where

import qualified Control.Exception as Ex
import           GHC.Stack (HasCallStack)

data Unimplemented
  = FprintfFile
  | GetHostNameNegativeSize
  | GetHostNameSmallSize
  | GetTimeOfDayNonNullTz
  | RecvFlagNonZero
  | RecvSymbolicLen
  | SendFlagNonZero
  | SendSymbolicLen
  | TimeLocNonNull
  deriving (Eq, Ord, Show)

ppUnimplemented :: Unimplemented -> String
ppUnimplemented =
  \case
    FprintfFile -> "`fprintf` called on file, not stdout or stderr"
    GetHostNameNegativeSize -> "`gethostname` called with a negative length"
    GetHostNameSmallSize -> "`gethostname` called with a small length"
    GetTimeOfDayNonNullTz -> "`gettimeofday` called with non-null tz"
    RecvFlagNonZero -> "`recv` called with a non-zero flag"
    RecvSymbolicLen -> "`recv` called with symbolic length"
    SendFlagNonZero -> "`send` called with a non-zero flag"
    SendSymbolicLen -> "`send` called with symbolic length"
    TimeLocNonNull -> "`time` non-null tloc"

instance Ex.Exception Unimplemented where

throw :: HasCallStack => Unimplemented -> a
throw = Ex.throw
