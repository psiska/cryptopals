module Cryptopals.Util.Codec.Strict where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import Cryptopals.Util.Codec.Lazy     as CL
import           Data.Word (Word8)
import           Protolude

-- | xor 2 bytestring, where fist server as a key, second as a content to be xored.
xorByteStrings :: B.ByteString  -- ^ key
               -> B.ByteString  -- ^ xored content
               -> B.ByteString  -- ^ result
xorByteStrings key content = BL.toStrict $ CL.xorByteStrings (BL.fromStrict key) (BL.fromStrict content)

-- Xor bytestring with a single byte key
xorWithWord8 :: Word8         -- ^ single byte key
             -> B.ByteString -- ^ xored content
             -> B.ByteString -- ^ result
xorWithWord8 key content = BL.toStrict $ CL.xorWithWord8 key (BL.fromStrict content)
