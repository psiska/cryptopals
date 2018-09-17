{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge2 where

import qualified Cryptopals.Util as U

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16

input :: B.ByteString
input = "1c0111001f010100061a024b53535009181c"

key :: B.ByteString
key = "686974207468652062756c6c277320657965"

-- | Fixed XOR
-- >>> :set -XOverloadedStrings
-- >>> challenge2 "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"
-- "746865206b696420646f6e277420706c6179"
challenge2 :: B.ByteString -> B.ByteString -> B.ByteString
challenge2 source key' =
  let dKey    = (fst . B16.decode) key'
      dSource = (fst . B16.decode) source
  in B16.encode $ U.xorDecode dKey dSource

