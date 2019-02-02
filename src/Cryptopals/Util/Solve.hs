{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Cryptopals.Util.Solve where

import qualified Data.ByteString as B
import           Data.List       (sortBy, uncons, (!!))
import           Data.Maybe      (catMaybes)
import           Data.Word       (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Prettyprint.Doc
import           Cryptopals.Util.Codec
import qualified Cryptopals.Util.Codec.Strict as CS
import           Cryptopals.Util.Freq
import qualified Cryptopals.Util.Freq as F
import           Cryptopals.Util.Data
import           Protolude


singleEntryAnalysis :: B.ByteString -> Word8 -> EntryAnalysis
singleEntryAnalysis source key =
  let decoded = CS.xorWithWord8 key source
      fv = freqVector decoded
      validCharCount = length $ B.findIndices isEngChar decoded
      charPercentage = (fromIntegral validCharCount) / fromIntegral (B.length decoded) * 100.0
      distance = Distance
        { euclidean = euclideanDistance fv englishVectorSorted
        , manhattan = manhattanDistance fv englishVectorSorted
        , minkowski = minkowskiDistance 2 fv englishVectorSorted
        , cosine = cosineSimilarity fv englishVectorSorted
        , jaccard = jaccardSimilarity  fv englishVectorSorted
        }
  in EntryAnalysis
    { key = [key]
    , decrypted = decoded
    , validChar = validCharCount
    , charPercentage = charPercentage
    , distance = distance
    }

solveSingleXorKey :: B.ByteString -> TextAnalysis
solveSingleXorKey source =
  TextAnalysis
    { sourceData = source
    , results = map (singleEntryAnalysis source) singleByteKeys
    }

filteredAndCharSorted :: TextAnalysis -> TextAnalysis
filteredAndCharSorted TextAnalysis { sourceData = s', results = r' } =
  TextAnalysis
    { sourceData = s'
    , results = ((sBy preferCharCount) . fPosCosine) r'
    }

xorResultFull :: B.ByteString -> TextAnalysis
xorResultFull = filteredAndCharSorted . solveSingleXorKey

-- TODO return top 5 key variants
-- TODO continue here
-- TODO Challenge6
solveXorKey :: Int -> B.ByteString -> [TextAnalysis]
solveXorKey size input =
  --let fullResult ::
  let fullResult = ((map xorResultFull) . B.transpose . (sizedChunks size)) input
      topFive = map (\TextAnalysis {sourceData = s', results = r'} -> TextAnalysis {sourceData = s', results = take 5 r'}) fullResult
  in topFive

--keyFromAnalysis :: TextAnalysis -> B.ByteString
--keyFromAnalysis TextAnalysis {results = r} = B.concat $ map B.pack r

-- TODO rename to something meaningful
ta2ar :: TextAnalysis -> B.ByteString -> AnalysisResult
ta2ar TextAnalysis {sourceData, results} k =
  AnalysisResult
    { inputData = sourceData
    , keyUsed = B.concat $ map (B.pack . key) results
    , resultData = CS.xorByteStrings k sourceData
    }

-- TODO Challenge6
-- TODO is this used ?? remove if not
keyFromAnalysis :: [EntryAnalysis] -> B.ByteString
keyFromAnalysis entries = B.pack $ foldl (\b EntryAnalysis{key = k} -> b <> k) [] entries

-- Key
--
-- TODO new param - size of chunks to take in account when determining keySize
properKeySize :: (Int, Int) -> B.ByteString -> [(Int, Float)]
properKeySize (lower, upper) content =
  let sorter = (\x y -> compare (snd x) (snd y))
  in sortBy sorter $ catMaybes $ fmap (hammingForKS content) [lower..upper]

hammingForKS :: B.ByteString -> Int -> Maybe (Int, Float)
hammingForKS input ks =
  if B.length input < ks * chunkCount
    then Nothing
    else Just (ks, average hamming / fromIntegral ks)
  where
    chunkCount :: Int
    chunkCount = 4
    -- create chunks
    chunks = sizedChunksTake ks chunkCount input
    -- create permutations
    permutations = subsequencesOfSize 2 chunks
    -- compute hamming
    hamming = fmap (\x -> hammingDistance (x !! 0) (x !! 1)) permutations
    average :: [Int] -> Float
    average it = (fromIntegral (sum it) / fromIntegral chunkCount )