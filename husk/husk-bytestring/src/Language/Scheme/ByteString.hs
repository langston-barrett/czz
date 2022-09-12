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
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque0 BS.empty))
        )
      , ( "singleton"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.singleton))
        )
      , ( "pack"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.pack))
        )
      , ( "unpack"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.unpack))
        )
      -- Basic Interface
      , ( "cons"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.cons))
        )
      , ( "snoc"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.snoc))
        )
      , ( "append"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque2 BS.append))
        )
      , ( "head"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.head))
        )
      , ( "last"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.last))
        )
      , ( "tail"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.tail))
        )
      , ( "init"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaque1 BS.init))
        )
      , ( "null"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaqueArgs1 BS.null))
        )
      -- Instances
      , ( "eq"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaqueArgs2 ((==) @ByteString)))
        )
      , ( "neq"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaqueArgs2 ((/=) @ByteString)))
        )
      , ( "leq"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaqueArgs2 ((<=) @ByteString)))
        )
      , ( "show"
        , LST.CustFunc (Cust.evalHuskable (Cust.opaqueArgs1 (show @ByteString)))
        )
      ]
