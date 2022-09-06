{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Czz.Orphans () where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified What4.FunctionName as What4
import qualified What4.ProgramLoc as What4

--------------------------------------------------------------------------------
-- What4 JSON

$(AesonTH.deriveJSON
  Aeson.defaultOptions { Aeson.unwrapUnaryRecords = True }
  ''What4.FunctionName)
$(AesonTH.deriveJSON Aeson.defaultOptions ''What4.Position)
$(AesonTH.deriveJSON Aeson.defaultOptions ''What4.ProgramLoc)

--------------------------------------------------------------------------------
-- ByteString JSON

-- https://github.com/haskell/base64-bytestring

-- Copyright (c) 2010 Bryan O'Sullivan <bos@serpentine.com>
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

instance Aeson.ToJSON ByteString where
    toJSON = Aeson.toJSON . decodeUtf8 . B64.encode

instance Aeson.FromJSON ByteString where
    parseJSON o = either fail return . B64.decode . encodeUtf8 =<< Aeson.parseJSON o

instance Aeson.ToJSON LBS.ByteString where
    toJSON = Aeson.toJSON . decodeUtf8 . B64.encode . LBS.toStrict

instance Aeson.FromJSON LBS.ByteString where
    parseJSON o = either fail (return . LBS.fromStrict) . B64.decode . encodeUtf8 =<< Aeson.parseJSON o

instance Aeson.ToJSONKey ByteString
instance Aeson.FromJSONKey ByteString
