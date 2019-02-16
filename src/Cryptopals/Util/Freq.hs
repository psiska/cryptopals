{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
module Cryptopals.Util.Freq
  ( isEngChar
  , analyzeInput
  , hammingDistance
  , levenshtein
  , preferCharCount
  , preferSimilarity
  , fPosCosine
  , sBy
  )
  where

import           Data.Bits       (Bits(..), (.&.), xor)
import qualified Data.ByteString as B
import           Data.Char       (ord, toUpper)
import           Data.List       (sortBy, (!!))
import           Data.Ord        (Ordering(..))
import           Data.Word       (Word8)
import           Data.String (String)
import           Cryptopals.Util.Data
import           Cryptopals.Util.Codec.Strict (xorWithWord8)
import           Protolude

isEngChar :: Word8 -> Bool
isEngChar i =
     (i > 31 && i < 35)
  || (i > 37 && i < 40)
  || (i > 43 && i < 47)
  || (i > 63 && i < 91)
  || (i > 96 && i < 123)

-- | Count the letter frequency in provided source bytestring
-- Counts for both lower and upper case letter
letterFrequency :: B.ByteString -> [(Char, Double)]
letterFrequency source =
  let tl = B.length source
      charOcc :: B.ByteString -> Char -> Double
      charOcc text c = fromIntegral (B.count ((fromIntegral . ord) c) text)
      charFreq :: B.ByteString -> Char -> Double
      charFreq text c = (charOcc text c  + charOcc text (toUpper c)) / (fromIntegral tl) * 100
      f :: B.ByteString -> Char -> (Char, Double)
      f text c = (c, charFreq text c)
  in map (f source) ['a'..'z']

letterFrequencyByAlpha :: B.ByteString -> [(Char, Double)]
letterFrequencyByAlpha source = sortBy (\f s -> compare (fst f) (fst s)) (letterFrequency source)

-- | Frequencies definition of char and occurence for english language.
englishFrequencies :: [(Char, Double)]
englishFrequencies =
  [ ('e', 12.792), ('t', 9.056), ('a', 8.167), ('o', 7.507), ('i', 6.966), ('n', 6.749)
  , ('s',  6.327), ('h', 6.094), ('r', 5.987), ('d', 4.253), ('l', 4.025), ('c', 2.782)
  , ('u',  2.758), ('m', 2.406), ('w', 2.360), ('f', 2.228), ('g', 2.015), ('y', 1.974)
  , ('p',  1.929), ('b', 1.492), ('v', 0.978), ('k', 0.772), ('j', 0.153), ('x', 0.150)
  , ('q',  0.095), ('z', 0.074)
  ]

levenshtein :: String -> String -> Int
levenshtein s t =
    d!!(length s)!!(length t)
    where d = [[distance m n|n<-[0..length t]]|m<-[0..length s]]
          distance i 0 = i
          distance 0 j = j
          distance i j = minimum [d!!(i-1)!!j+1, d!!i!!(j-1)+1, d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then 0 else 1)]

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance input1 input2 =
  sqrt $ foldl (\acc (i1, i2) -> acc + ((i1 - i2) ** 2.0)) 0.0 (zip input1 input2)

manhattanDistance :: [Double] -> [Double] -> Double
manhattanDistance input1 input2 =
  foldl (\acc (i1, i2) -> acc + (abs (i1 - i2))) 0.0 (zip input1 input2)


minkowskiDistance :: Int -> [Double] -> [Double] -> Double
minkowskiDistance pVal input1 input2 =
  let s = foldl (\acc (i1, i2) -> acc + ((abs (i1 - i2)) ** (fromIntegral pVal))) 0.0 (zip input1 input2)
  in s ** (1 / (fromIntegral pVal))


-- TODO test length
cosineSimilarity :: [Double] -> [Double] -> Double
cosineSimilarity input1 input2 =
  let sums = foldl
               (\(accTop, accA, accB) (i1, i2) -> ((accTop + (i1 * i2)), accA + (i1 ** 2.0), accB + (i2 ** 2.0)))
               (0.0, 0.0, 0.0)
               (zip input1 input2)
  in (\(top, a, b) -> top / ((sqrt a) * (sqrt b))) sums

jaccardSimilarity :: [Double] -> [Double] -> Double
jaccardSimilarity input1 input2 =
  let inter op x1 x2
        | (x1 /= 0.0) `op` (x2 /= 0.0) = 1
        | otherwise                    = 0
      sums = foldl
        (\(accIntersection, accCard) (i1, i2) -> (accIntersection + inter (||) i1 i2, accCard + inter (&&) i1 i2))
        (0, 0)
        (zip input1 input2)
  in (\(intersection, cardinality) -> if cardinality == 0 then 0 else intersection / cardinality) sums

-- | Hamming Distance
-- >>> :set -XOverloadedStrings
-- >>> hammingDistance "this is a test" "wokka wokka!!!"
-- 37
hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance i1 i2 =
  let setBits :: (Num a, Bits a) => a -> Int
      setBits x
        | x == 0    = 0
        | otherwise = 1 + setBits (x .&. (x - 1))
  in foldl (\acc (w1, w2) -> acc + setBits (xor w1 w2)) 0 (B.zip i1 i2)

-- | Analyze byte block with single Word8 as a key.
-- TODO in future maybe provide way how to just compare two ByteString - thus factor out the xorWithWord8
analyzeInput :: B.ByteString -> Word8 -> DecodeAnalysis
analyzeInput source key =
  let decoded = xorWithWord8 key source
      -- obtain vector of frequencies for source
      fv = map snd $ letterFrequencyByAlpha decoded
      -- obtain vector of frequencies for english text
      englishFv = map snd $ sortBy (\f s -> compare (fst f) (fst s)) englishFrequencies
      validCharCount = length $ B.findIndices isEngChar decoded
      distance = Distance
        { euclidean = euclideanDistance fv englishFv
        , manhattan = manhattanDistance fv englishFv
        , minkowski = minkowskiDistance 2 fv englishFv
        , cosine = cosineSimilarity fv englishFv
        , jaccard = jaccardSimilarity  fv englishFv
        }
  in DecodeAnalysis
    { key = [key]
    , decrypted = decoded
    , validChar = validCharCount
    , charPercentage = fromIntegral validCharCount / fromIntegral (B.length decoded) * 100.0
    , distance = distance
    }

-- | sim and count. Sort by count and then by sim.
preferCharCount :: (Ordering, Ordering) -> Ordering
preferCharCount (_, GT) = LT
preferCharCount (x, EQ) = x
preferCharCount (_, LT) = GT

preferSimilarity :: (Ordering, Ordering) -> Ordering
preferSimilarity (GT, _) = LT
preferSimilarity (EQ, EQ) = EQ
preferSimilarity (EQ, LT) = GT
preferSimilarity (EQ, GT) = LT
preferSimilarity (LT, _) = GT

fPosCosine :: [DecodeAnalysis] -> [DecodeAnalysis]
fPosCosine = filter (\x -> (((getField @"cosine") . (getField @"distance")) x) > 0.0)

sBy :: ((Ordering, Ordering) -> Ordering) -> [DecodeAnalysis] -> [DecodeAnalysis]
sBy sorting = let sorter = (\x y ->
                    sorting (compare (cosSim x) (cosSim y), compare (charPercentage x) (charPercentage y)))
                      where cosSim = getField @"cosine" . getField @"distance"

           in sortBy sorter
