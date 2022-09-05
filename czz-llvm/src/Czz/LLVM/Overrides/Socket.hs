{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Czz.LLVM.Overrides.Socket
  ( Effect(..)
  , AcceptEffect(..)
  , BindEffect(..)
  , ListenEffect(..)
  , RecvEffect(..)
  , SocketEffect(..)
  , overrides
  )
where

import qualified Control.Lens as Lens
import           Control.Monad.IO.Class (liftIO)
import qualified Data.BitVector.Sized as BV
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.IORef (IORef)
import qualified System.Random as Random

import           Data.Parameterized.Context ((::>), EmptyCtx)
import           Data.Parameterized.NatRepr (knownNat)

import qualified What4.Interface as What4

-- crucible
import           Lang.Crucible.Backend (IsSymBackend)
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.Simulator.RegMap as C
import           Lang.Crucible.Simulator.OverrideSim (OverrideSim)
import qualified Lang.Crucible.Simulator as C
import           Lang.Crucible.Simulator.RegMap (RegEntry, RegValue)
import           Lang.Crucible.Types (BVType)

-- crucible-llvm
import qualified Lang.Crucible.LLVM.DataLayout as CLLVM
import           Lang.Crucible.LLVM.Intrinsics (OverrideTemplate, LLVMOverride)
import qualified Lang.Crucible.LLVM.Intrinsics as CLLVM
import qualified Lang.Crucible.LLVM.MemModel as CLLVM
import           Lang.Crucible.LLVM.MemModel.Pointer (LLVMPointerType)

import qualified Czz.Log as Log
import           Czz.Overrides (EffectTrace, Override)
import qualified Czz.Overrides as COv
import qualified Czz.Random as Rand

import           Czz.LLVM.Overrides.Util (OverrideConstraints)
import           Czz.LLVM.QQ (llvmArgs, llvmOvr, llvmOvrType)
import qualified Czz.LLVM.Unimplemented as Unimpl

data Effect
  = Accept !AcceptEffect
  | Bind !BindEffect
  | Listen !ListenEffect
  | Recv !RecvEffect
  | Send !SendEffect
  | SetSockOpt !SetSockOptEffect
  | Socket !SocketEffect
  deriving (Eq, Ord, Show)

_Accept :: Lens.Prism' Effect AcceptEffect
_Accept =
  Lens.prism'
    Accept
    (\case
      Accept eff -> Just eff
      _ -> Nothing)

_Bind :: Lens.Prism' Effect BindEffect
_Bind =
  Lens.prism'
    Bind
    (\case
      Bind eff -> Just eff
      _ -> Nothing)

_Listen :: Lens.Prism' Effect ListenEffect
_Listen =
  Lens.prism'
    Listen
    (\case
      Listen eff -> Just eff
      _ -> Nothing)

_Recv :: Lens.Prism' Effect RecvEffect
_Recv =
  Lens.prism'
    Recv
    (\case
      Recv eff -> Just eff
      _ -> Nothing)

_Send :: Lens.Prism' Effect SendEffect
_Send =
  Lens.prism'
    Send
    (\case
      Send eff -> Just eff
      _ -> Nothing)

_SetSockOpt :: Lens.Prism' Effect SetSockOptEffect
_SetSockOpt =
  Lens.prism'
    SetSockOpt
    (\case
      SetSockOpt eff -> Just eff
      _ -> Nothing)

_Socket :: Lens.Prism' Effect SocketEffect
_Socket =
  Lens.prism'
    Socket
    (\case
      Socket eff -> Just eff
      _ -> Nothing)

overrides ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff Effect ->
  [OverrideTemplate p sym arch rtp l a]
overrides proxy effects inj =
  [ ov (acceptDecl proxy effects (inj . _Accept))
  , ov (bindDecl proxy effects (inj . _Bind))
  , ov (listenDecl proxy effects (inj . _Listen))
  , ov (recvDecl proxy effects (inj . _Recv))
  , ov (sendDecl proxy effects (inj . _Send))
  , ov (setSockOptDecl proxy effects (inj . _SetSockOpt))
  , ov (socketDecl proxy effects (inj . _Socket))
  ]
  where ov = CLLVM.basic_llvm_override

------------------------------------------------------------------------
-- ** Helpers (not exported)

n32 :: What4.NatRepr 32
n32 = knownNat @32

doAssert ::
  IsSymBackend sym bak =>
  bak ->
  String ->
  What4.Pred sym ->
  OverrideSim p sym ext rtp args ret ()
doAssert bak msg p =
  liftIO (C.assert bak p (C.AssertFailureSimError msg msg))

-- | Unsound! This should be coordinated with the symio/filesystem stuff, it's
-- supposed to be "the lowest-numbered file descriptor not currently open for
-- the process", so this will get messed up if user code does a comparison
-- between various file descriptors...
socketFd :: Integer
socketFd = 1234567

-- | Not yet sound, but better than nothing...
assertIsSocketFd ::
  IsSymBackend sym bak =>
  bak ->
  -- | Function name
  String ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret ()
assertIsSocketFd bak fnName sockFd = do
  let sym = C.backendGetSym bak
  doAssert bak ("`" ++ fnName ++ "` called on negative fd") =<<
    liftIO (What4.bvSgt sym (C.regValue sockFd) =<<
              What4.bvLit sym n32 (BV.mkBV n32 0))

  doAssert bak ("`" ++ fnName ++ "` called on non-socket") =<<
    liftIO (What4.bvEq sym (C.regValue sockFd) =<<
              What4.bvLit sym n32 (BV.mkBV n32 socketFd))

------------------------------------------------------------------------
-- ** accept

data AcceptEffect
  = AcceptSuccess
  deriving (Eq, Ord, Show)

acceptDecl ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff AcceptEffect ->
  LLVMOverride
    p
    sym
    (EmptyCtx ::> BVType 32
              ::> LLVMPointerType wptr
              ::> LLVMPointerType wptr)
    (BVType 32)
acceptDecl proxy effects inj =
  [llvmOvr| i32 @accept( i32, %struct.sockaddr*, i32* ) |]
  (\memVar bak args ->
    let ov = acceptOverride proxy bak memVar
    in COv.toOverride effects inj args ov)

acceptOverride ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  Override
    sym
    bak
    AcceptEffect
    (EmptyCtx
     ::> BVType 32
     ::> LLVMPointerType wptr
     ::> LLVMPointerType wptr)
    (BVType 32)
acceptOverride proxy bak memVar =
  COv.Override
  { COv.genEffect = \_proxy _oldEff _sockFd _addr _addrLen ->
      COv.AnyOverrideSim (return AcceptSuccess)
  , COv.doEffect = \_proxy e sockFd addr addrLen ->
      COv.AnyOverrideSim (acceptImpl proxy bak e memVar sockFd addr addrLen)
  }

-- | Unsound!
--
-- TODO(lb): also generate error conditions
acceptImpl ::
  IsSymBackend sym bak =>
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  bak ->
  AcceptEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (LLVMPointerType wptr) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
acceptImpl _proxy bak e _memVar sockFd _addr _addrLen = do
  AcceptSuccess <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  -- TODO(lb): Set errno to ENOTSOCK or EBADF in this case, return -1
  assertIsSocketFd bak "accept" sockFd
  -- TODO(lb): Needs to return new fd
  liftIO (What4.bvLit sym n32 (BV.mkBV n32 0))

------------------------------------------------------------------------
-- ** bind

data BindEffect
  = BindSuccess
  deriving (Eq, Ord, Show)

bindDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff BindEffect ->
  [llvmOvrType| i32 @( i32, %struct.sockaddr*, i32 ) |]
bindDecl proxy effects inj =
  [llvmOvr| i32 @bind( i32, %struct.sockaddr*, i32 ) |]
  (\memVar bak args ->
    let ov = bindOverride proxy bak memVar
    in COv.toOverride effects inj args ov)

bindOverride ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  Override sym bak BindEffect [llvmArgs| i32, %struct.sockaddr*, i32 |] (BVType 32)
bindOverride proxy bak memVar =
  COv.Override
  { COv.genEffect =
      \_proxy _oldEff _sockFd _addr _addrLen ->
        COv.AnyOverrideSim (return BindSuccess)
  , COv.doEffect =
      \_proxy e sockFd addr addrLen ->
        COv.AnyOverrideSim (bindImpl proxy bak e memVar sockFd addr addrLen)
  }

-- | Unsound!
--
-- TODO(lb): also generate error conditions
bindImpl ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  BindEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
bindImpl _proxy bak e _memVar sockFd _addr _addrLen = do
  BindSuccess <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  -- TODO(lb): Set errno to EBADF in this case, return -1
  assertIsSocketFd bak "bind" sockFd
  liftIO (What4.bvLit sym n32 (BV.mkBV n32 0))

------------------------------------------------------------------------
-- ** listen

data ListenEffect
  = ListenSuccess
  deriving (Eq, Ord, Show)

listenDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff ListenEffect ->
  [llvmOvrType| i32 @( i32, i32 ) |]
listenDecl proxy effects inj =
  [llvmOvr| i32 @listen( i32, i32 ) |]
  (\memVar bak args ->
    let ov = listenOverride proxy bak memVar
    in COv.toOverride effects inj args ov)

listenOverride ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  Override sym bak ListenEffect [llvmArgs| i32, i32 |] (BVType 32)
listenOverride proxy bak memVar =
  COv.Override
  { COv.genEffect = \_proxy _oldEff _sockFd _backlog ->
      COv.AnyOverrideSim (return ListenSuccess)
  , COv.doEffect = \_proxy e sockFd backlog ->
      COv.AnyOverrideSim (listenImpl proxy bak e memVar sockFd backlog)
  }

-- | Unsound!
--
-- TODO(lb): also generate error conditions
listenImpl ::
  IsSymBackend sym bak =>
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  bak ->
  ListenEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
listenImpl _proxy bak e _memVar sockFd _backlog = do
  ListenSuccess <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  -- TODO(lb): Set errno to ENOTSOCK in this case, return -1
  assertIsSocketFd bak "listen" sockFd
  liftIO (What4.bvLit sym n32 (BV.mkBV n32 0))

------------------------------------------------------------------------
-- ** recv

data RecvEffect
  = RecvSuccess !ByteString
  deriving (Eq, Ord, Show)

recvDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff RecvEffect ->
  [llvmOvrType| ssize_t @(i32, i8*, size_t, i32) |]
recvDecl proxy effects inj =
  [llvmOvr| ssize_t @recv(i32, i8*, size_t, i32) |]
  (\memVar bak args ->
    let ov = recvOverride proxy bak memVar
    in COv.toOverride effects inj args ov)

recvOverride ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  C.GlobalVar CLLVM.Mem ->
  Override sym bak RecvEffect [llvmArgs| i32, i8*, size_t, i32 |] (BVType wptr)
recvOverride proxy bak memVar =
  COv.Override
    -- TODO(lb): mutate oldEff
  { COv.genEffect = \_proxy _oldEff _sockFd _buf len _flags ->
      COv.AnyOverrideSim $ liftIO $ do
        lenBv <-
          case What4.asBV (C.regValue len) of
            Nothing -> Unimpl.throw Unimpl.RecvSymbolicLen
            Just bv -> return bv
        let lenInteger = BV.asUnsigned lenBv
        let lenInt = fromIntegral lenInteger
        recvd <-
          liftIO (Random.randomRIO (0, (lenInt `div` 8) - 1) :: IO Int)
        str <- Rand.genByteString (0, recvd)  -- inclusive
        return (RecvSuccess str)
  , COv.doEffect =
      \_proxy e  sockFd buf len flags ->
        COv.AnyOverrideSim (recvImpl proxy bak e memVar sockFd buf len flags)
  }

-- | Unsound!
--
-- TODO(lb): also generate error conditions
recvImpl ::
  IsSymBackend sym bak =>
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  bak ->
  RecvEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (BVType wptr) ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType wptr))
recvImpl _proxy bak e memVar sockFd buf len flags = do
  RecvSuccess recvd <- return e
  let sym = C.backendGetSym bak
  -- TODO(lb): Set errno to EBADF in this case, return -1
  assertIsSocketFd bak "recv" sockFd
  let lenIsSymbolic =
        case What4.asBV (C.regValue len) of
          Nothing -> What4.truePred sym
          Just _ -> What4.falsePred sym
  flagNonZero <-
    liftIO (What4.bvNe sym (C.regValue flags) =<<
              What4.bvLit sym n32 (BV.mkBV n32 0))
  C.symbolicBranches
    C.emptyRegMap
    [ ( lenIsSymbolic
      , Unimpl.throw Unimpl.RecvSymbolicLen
      , Nothing
      )
    , ( flagNonZero
      , Unimpl.throw Unimpl.RecvFlagNonZero
      , Nothing
      )
    , ( What4.truePred sym
      , doRecv sym recvd
      , Nothing
      )
    ]
  where
    doRecv sym bs =
      C.modifyGlobal memVar $ \mem -> liftIO $ do
        let val = CLLVM.LLVMValString bs
        let ty = CLLVM.llvmValStorableType val
        let bsLen = BS.length bs
        bsLenExpr <- What4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth (fromIntegral bsLen))
        let buf' = C.regValue buf
        mem' <- liftIO (CLLVM.storeRaw bak mem buf' ty CLLVM.noAlignment val)
        return (bsLenExpr, mem')

------------------------------------------------------------------------
-- ** send

data SendEffect
  = SendSuccess !Integer !Integer
  deriving (Eq, Ord, Show)

sendDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff SendEffect ->
  [llvmOvrType| size_t @(i32, i8*, size_t, i32) |]
sendDecl proxy effects inj =
  [llvmOvr| size_t @send(i32, i8*, size_t, i32) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType wptr)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _sockFd _buf len _flags ->
           COv.AnyOverrideSim $ do
             case What4.asBV (C.regValue len) of
               Nothing -> Unimpl.throw Unimpl.SendSymbolicLen
               Just bv ->
                 let len' = BV.asUnsigned bv
                 in SendSuccess len' <$>
                     liftIO (Random.randomRIO (0, len') :: IO Integer)
       , COv.doEffect = \_proxy e sockFd buf len flags ->
           COv.AnyOverrideSim (sendImpl proxy bak e memVar sockFd buf len flags)
       }))

-- | Unsound!
--
-- TODO(lb): also generate error conditions
sendImpl ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  SendEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (BVType wptr) ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType wptr))
sendImpl _proxy bak e memVar sockFd buf len flags = do
  SendSuccess lenInteger sent <- return e
  let sym = C.backendGetSym bak
  -- TODO(lb): Set errno to EBADF in this case, return -1
  assertIsSocketFd bak "send" sockFd
  let lenIsSymbolic =
        case What4.asBV (C.regValue len) of
          Nothing -> What4.truePred sym
          Just _ -> What4.falsePred sym
  flagNonZero <-
    liftIO (What4.bvNe sym (C.regValue flags) =<<
              What4.bvLit sym n32 (BV.mkBV n32 0))
  C.symbolicBranches
    C.emptyRegMap
    [ ( lenIsSymbolic
      , Unimpl.throw Unimpl.SendSymbolicLen
      , Nothing
      )
    , ( flagNonZero
      , Unimpl.throw Unimpl.SendFlagNonZero
      , Nothing
      )
    , ( What4.truePred sym,
        C.modifyGlobal memVar $ \mem -> liftIO $ do
          let lenInt = fromIntegral lenInteger
          -- TODO(lb): no reason for this to fail... make a variant that returns
          -- predicates rather than asserting
          let buf' = C.regValue buf
          output <-
            BS.pack <$> liftIO (CLLVM.loadString bak mem buf' (Just lenInt))
          liftIO (Log.debug ("Program called `send`: " ++ show output))
          sentExpr <- What4.bvLit sym ?ptrWidth (BV.mkBV ?ptrWidth sent)
          return (sentExpr, mem)
      , Nothing
      )
    ]

------------------------------------------------------------------------
-- ** setsockopt

data SetSockOptEffect
  = SetSockOptSuccess
  deriving (Eq, Ord, Show)

setSockOptDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff SetSockOptEffect ->
  [llvmOvrType| i32 @( i32, i32, i32, i8*, i32 ) |]
setSockOptDecl proxy effects inj =
  [llvmOvr| i32 @setsockopt( i32, i32, i32, i8*, i32 ) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType 32)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect =
           \_proxy _oldEff _sockFd _level _optName _optVal _optLen ->
             COv.AnyOverrideSim (return SetSockOptSuccess)
       , COv.doEffect =
           \_proxy e sockFd level optName optVal optLen ->
             COv.AnyOverrideSim (setSockOptImpl proxy bak e memVar sockFd level optName optVal optLen)
       }))

-- | Unsound!
--
-- TODO(lb): also generate error conditions
setSockOptImpl ::
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  IsSymBackend sym bak =>
  proxy arch ->
  bak ->
  SetSockOptEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (LLVMPointerType wptr) ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
setSockOptImpl _proxy bak e _memVar sockFd _level _optName _optVal _optLen = do
  SetSockOptSuccess <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak
  -- TODO(lb): Set errno to EBADF in this case, return -1
  assertIsSocketFd bak "setsockopt" sockFd
  liftIO (What4.bvLit sym n32 (BV.mkBV n32 0))

------------------------------------------------------------------------
-- ** socket

data SocketEffect
  = SocketSuccess !Integer
  deriving (Eq, Ord, Show)

socketDecl ::
  forall proxy p sym arch wptr eff.
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  IORef (EffectTrace eff) ->
  Lens.Prism' eff SocketEffect ->
  [llvmOvrType| i32 @( i32, i32, i32 ) |]
socketDecl proxy effects inj =
  [llvmOvr| i32 @socket( i32, i32, i32 ) |]
  (\memVar bak args ->
    COv.toOverride
      @(BVType 32)
      effects
      inj
      args
      (COv.Override
       { COv.genEffect = \_proxy _oldEff _domain _type _protocol ->
           COv.AnyOverrideSim (return (SocketSuccess socketFd))
       , COv.doEffect = \_proxy e domain type_ protocol ->
           COv.AnyOverrideSim (socketImpl proxy bak e memVar domain type_ protocol )
       }))

-- | Unsound!
--
-- TODO(lb): also generate error conditions
socketImpl ::
  IsSymBackend sym bak =>
  Log.Has String =>
  OverrideConstraints sym arch wptr =>
  proxy arch ->
  bak ->
  SocketEffect ->
  C.GlobalVar CLLVM.Mem ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (BVType 32) ->
  RegEntry sym (BVType 32) ->
  OverrideSim p sym ext rtp args ret (RegValue sym (BVType 32))
socketImpl _proxy bak e _memVar domain type_ protocol = do
  SocketSuccess fd <- return e  -- check for missing pattern matches
  let sym = C.backendGetSym bak

  -- include/linux/socket.h: AF_INET = 2
  let afInet = 2
  doAssert bak "`socket` only supports domain AF_INET" =<<
    liftIO (What4.bvEq sym (C.regValue domain) =<<
              What4.bvLit sym n32 (BV.mkBV n32 afInet))

  -- include/linux/net.h: SOCK_STREAM = 2
  let sockStream = 1
  doAssert bak "`socket` only supports type SOCK_STREAM" =<<
    liftIO (What4.bvEq sym (C.regValue type_) =<<
              What4.bvLit sym n32 (BV.mkBV n32 sockStream))

  doAssert bak "`socket` only supports protocol 0" =<<
    liftIO (What4.bvEq sym (C.regValue protocol) =<<
              What4.bvLit sym n32 (BV.mkBV n32 0))

  liftIO (What4.bvLit sym n32 (BV.mkBV n32 fd))
