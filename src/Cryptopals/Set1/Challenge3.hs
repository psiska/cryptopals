{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge3 where

import qualified Cryptopals.Util as U

import qualified Data.ByteString as B

input :: B.ByteString
input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- | Single byte xor cypher
-- >>> :set -XOverloadedStrings
-- >>> challenge3 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
-- Just (88,0.67861676,"Cooking MC's like a pound of bacon")
-- TODO fix test
challenge3 :: B.ByteString -> Maybe U.XorFinding
challenge3 = U.findSingleXor
