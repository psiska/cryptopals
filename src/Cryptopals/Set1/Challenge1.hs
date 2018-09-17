{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge1 where

import qualified Cryptopals.Util.Hex as CPH

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char (chr)

input :: B.ByteString
input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

-- | Hex to base64
-- | https://cryptopals.com/sets/1/challenges/1
-- >>> :set -XOverloadedStrings
-- >>> challenge1 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
-- "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
challenge1 :: B.ByteString -> B.ByteString
challenge1 = B64.encode . fst . B16.decode

-- | Hex to base64
-- | https://cryptopals.com/sets/1/challenges/1
-- >>> :set -XOverloadedStrings
-- >>> challenge1' "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
-- "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
challenge1' :: String -> BC.ByteString
challenge1' = B64.encode . BC.pack . (map chr) . CPH.sFromHex
