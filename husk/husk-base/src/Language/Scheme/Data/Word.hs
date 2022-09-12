module Language.Scheme.Data.Word
  ( extendEnv
  )
where

import qualified Data.Word as Word

import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import qualified Language.Scheme.CustFunc as Cust
import           Language.Scheme.To ()

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e =
  LSV.extendEnv
    e
    (map (\(nm, f) -> ((LSV.varNamespace, pfx ++ "-" ++ nm), f)) funcs)
  where
    funcs =
      [ ("bitReverse8"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 Word.bitReverse8)))
      ]
