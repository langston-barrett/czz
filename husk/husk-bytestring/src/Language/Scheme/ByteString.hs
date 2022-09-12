{-# LANGUAGE TypeApplications #-}
module Language.Scheme.ByteString
  ( extendEnv
  )
where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Language.Scheme.Types as LST
import qualified Language.Scheme.Variables as LSV

import           Language.Scheme.CustFunc as Cust
import           Language.Scheme.Opaque (Opaque(..))  -- for auto

extendEnv :: String -> LST.Env -> IO LST.Env
extendEnv pfx e =
  LSV.extendEnv
    e
    (map (\(nm, f) -> ((LSV.varNamespace, pfx ++ "-" ++ nm), f)) funcs)
  where
    funcs =
      [
      -- Introducing and elminating
        ( "empty"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.empty))
        )
      , ( "singleton"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.singleton))
        )
      , ( "pack"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.pack))
        )
      , ( "unpack"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.unpack))
        )
      -- Basic Interface
      , ( "cons"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.cons))
        )
      , ( "snoc"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.snoc))
        )
      , ( "append"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.append))
        )
      , ( "head"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.head))
        )
      , ( "last"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.last))
        )
      , ( "tail"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.tail))
        )
      , ( "init"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.init))
        )
      , ( "null"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto BS.null))
        )
      -- Instances
      , ( "eq"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto ((==) @ByteString)))
        )
      , ( "neq"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto ((/=) @ByteString)))
        )
      , ( "leq"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto ((<=) @ByteString)))
        )
      , ( "show"
        , LST.CustFunc (Cust.evalHuskable (Cust.auto (show @ByteString)))
        )
      ]
