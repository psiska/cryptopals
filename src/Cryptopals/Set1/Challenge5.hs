{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge5 where

import qualified Cryptopals.Util as U

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Protolude

inputText :: B.ByteString
inputText = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

inputKey :: B.ByteString
inputKey = "ICE"


-- | Repeating-key XOR encoding
-- >>> :set -XOverloadedStrings
-- >>> challenge5 inputKey inputText
-- "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
--
-- 0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272
-- a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f
challenge5 :: B.ByteString -> B.ByteString -> B.ByteString
challenge5 key = B16.encode . (U.xorByteStrings key)
