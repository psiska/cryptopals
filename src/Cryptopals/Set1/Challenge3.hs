{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge3 where

import qualified Cryptopals.Util as U

import qualified Data.ByteString as B
import           Data.Maybe (listToMaybe)
import           Protolude

input :: B.ByteString
input = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

-- | Single byte xor cypher
-- >>> :set -XOverloadedStrings
-- >>> challenge3 "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
-- Just (EntryAnalysis {key = [88], sourceData = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736", deHex = "\ESC77316?x\NAK\ESC\DEL+x413=x9x(7-6<x7>x:9;76", decrypted = "Cooking MC's like a pound of bacon", validChar = 34, charPercentage = 100.0, distance = Distance {euclidean = 20.169820589496, manhattan = 71.1659411764706, minkowsi = 20.169820589496, cosine = 0.6684454853665066, jaccard = 1.625}})
challenge3 :: B.ByteString -> Maybe U.EntryAnalysis
challenge3 = listToMaybe . U.results . U.xorResultFull

challenge3Full :: B.ByteString -> U.TextAnalysis
challenge3Full = U.xorResultFull
