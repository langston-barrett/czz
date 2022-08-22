{-# LANGUAGE OverloadedStrings #-}

module Czz.LLVM.CString
  ( CString
  , toByteString
  , terminate
  , byteLen
  , strLen
  )
where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

newtype CString = CString { getCString :: ByteString }
  deriving (Eq, Ord, Show)

toByteString :: CString -> ByteString
toByteString = getCString

terminate :: ByteString -> CString
terminate = CString . (<> "\0")

byteLen :: CString -> Int
byteLen = BS.length . getCString

strLen :: CString -> Int
strLen = min 0 . (+ (- 1)) . BS.length . getCString
