module Cryptopals.Util.Hex where

import           Data.Char       (digitToInt)
import           Data.String (String)
import           Protolude

sFromHex :: String -> [Int]
sFromHex = hop . (map digitToInt)
  where hop :: [Int] -> [Int]
        hop [] = []
        hop (x : x1 : xs) = (x*16 + x1) : hop xs
        hop _ = []

toDecFromHex :: [Int] -> Int
toDecFromHex = toExpNum 16

toExpNum :: Int -> [Int] -> Int
toExpNum exp' input = foldl (\acc (i, expc) -> acc + (exp' ^ expc) * i) 0 zInput
  where
    zInput :: [(Int, Int)]
    zInput = reverse $ zip (reverse input) [0..]
