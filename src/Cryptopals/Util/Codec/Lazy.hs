module Cryptopals.Util.Codec.Lazy where

import           Data.Bits (xor)
import           Data.Word (Word8)
import qualified Data.ByteString.Lazy as B
import           Protolude

-- | xor 2 bytestring, where fist server as a key, second as a content to be xored.
xorByteStrings :: B.ByteString  -- ^ key
               -> B.ByteString  -- ^ xored content
               -> B.ByteString  -- ^ result
xorByteStrings key content =
  let repeatedKey = key `B.append` repeatedKey
  in B.pack $ map (uncurry xor) (B.zip content repeatedKey)

-- Xor bytestring with a single byte key
xorWithWord8 :: Word8         -- ^ single byte key
             -> B.ByteString -- ^ xored content
             -> B.ByteString -- ^ result
xorWithWord8 key = xorByteStrings (B.singleton key)
