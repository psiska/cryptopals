module Cryptopals.Util.Codec
  ( module Cryptopals.Util.Codec.Strict
  , singleByteKeys
  , hex2raw
  , sizedChunks
  , sizedChunksTake
  , subsequencesOfSize
  --, module Lazy
  )
    where

--import Cryptopals.Util.Codec.Lazy
import Cryptopals.Util.Codec.Strict

import Data.Function (fix)
import Data.List ((!!))
import Data.Word (Word8)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import           Protolude

singleByteKeys :: [Word8]
singleByteKeys = enumFrom 0

hex2raw :: B.ByteString -> B.ByteString
hex2raw = fst . B16.decode

-- split input bytestring to even sized chunks defined by first arg
-- | sizedChunks 2 "dkf;alsjfkopevtjpvosjdfpovjdsf;lvdsfj;v"
-- >>> :set -XOverloadedStrings
-- >>> sizedChunks 2 "dkf;alsjfkopevtjpvosjdfpovjdsf;lvdsfj;v"
-- ["dk","f;","al","sj","fk","op","ev","tj","pv","os","jd","fp","ov","jd","sf",";l","vd","sf","j;","v"]
sizedChunks :: Int -> B.ByteString -> [B.ByteString]
sizedChunks size input =
  snd $ fix (\rec (i, acc) ->
    if B.length i > size
      then rec (B.drop size i, acc ++ [B.take size i])
      else (B.empty, acc ++ [i])) (input, [])

-- | sizedChunksTake
-- >>> :set -XOverloadedStrings
-- >>> sizedChunksTake 2 4 "dkf;alsjfkopevtjpvosjdfpovjdsf;lvdsfj;v"
-- ["dk","f;","al","sj"]
sizedChunksTake :: Int -> Int -> B.ByteString -> [B.ByteString]
sizedChunksTake size count input =
  snd $ fix (\rec (count, remainder, acc) ->
    if (B.length remainder > size && count > 0)
      then rec (count - 1, B.drop size remainder, acc ++ [B.take size remainder])
      else (B.empty, acc)) (count, input, [])

-- Various other utils
-- taken from https://stackoverflow.com/questions/21265454/subsequences-of-length-n-from-list-performance/21288092#21288092
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n > l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

