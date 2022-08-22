{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Option
  ( options
  , Verbosity(..)
  )
where

import           Data.Proxy (Proxy(Proxy))

import qualified Test.Tasty.Options as TastyO

options :: [TastyO.OptionDescription]
options =
  [ TastyO.Option (Proxy @Verbosity)
  ]

data Verbosity
  = None
  | All
  deriving Eq

instance TastyO.IsOption Verbosity where
  optionName = "verbosity"
  optionHelp = "'all' or 'none'"
  showDefaultValue =
    \case
      All -> Just "all"
      None -> Just "none"
  defaultValue = None
  parseValue =
    \case
      "all" -> Just All
      "none" -> Just None
      _ -> Nothing
