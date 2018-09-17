module Cryptopals.Util.Codec where

import           Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Word (Word8)

hex2raw :: B.ByteString -> B.ByteString
hex2raw = fst . B16.decode

singleByteKeys :: [Word8]
singleByteKeys = enumFrom 0

xorDecode :: B.ByteString -> B.ByteString -> B.ByteString
xorDecode key source = B.pack $ B.zipWith xor key source

xorDecode' :: Word8 -> B.ByteString -> B.ByteString
xorDecode' keyChar source =
  let key = B.replicate (B.length source) keyChar
  in B.pack $ B.zipWith xor key source

