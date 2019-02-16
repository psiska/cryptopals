{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Cryptopals.Util.Solve where

import qualified Data.ByteString as B
import           Data.List       (sortBy, (!!))
import           Data.Maybe      (mapMaybe)
import           Cryptopals.Util.Codec
import qualified Cryptopals.Util.Codec.Strict as CS
import           Cryptopals.Util.Freq
import           Cryptopals.Util.Data
import           Protolude


solveSingleXorKey :: B.ByteString -> DataAnalysis
solveSingleXorKey source =
  DataAnalysis
    { sourceData = source
    , results = map (analyzeInput source) singleByteKeys
    }

filteredAndCharSorted :: DataAnalysis -> DataAnalysis
filteredAndCharSorted DataAnalysis { sourceData = s', results = r' } =
  DataAnalysis
    { sourceData = s'
    , results = (sBy preferCharCount . fPosCosine) r'
    }

xorResultFull :: B.ByteString -> DataAnalysis
xorResultFull = filteredAndCharSorted . solveSingleXorKey

-- TODO try to make use of lenses
solveXorKey :: Int -> B.ByteString -> [DataAnalysis]
solveXorKey size input =
  let transposedChunks :: [B.ByteString]
      transposedChunks = B.transpose $ sizedChunks size input
      fullResult = map xorResultFull transposedChunks
      topFive = map (\DataAnalysis {sourceData, results = r'} -> DataAnalysis {sourceData, results = take 5 r'}) fullResult
      --tf = map (field @"results" .~ (take 5)) fullResult
  in topFive

-- | Taking output from  solveXorKey
keysFromData :: [DataAnalysis] -> [[Word8]]
keysFromData = transpose . map (concatMap key . results)

-- | Apply key to some data and show result
applyKey :: B.ByteString -> [Word8] -> AnalysisResult
applyKey input key =
  let bsKey = B.pack key
  in AnalysisResult
    { inputData = input
    , keyUsed = bsKey
    , resultData = CS.xorByteStrings bsKey input
    }

-- Key
--
-- | Try to find out what is good chunkSize
findKeySize :: (Int, Int) -> Int -> B.ByteString -> [(Int, Float)]
findKeySize (lower, upper) chunkCount content =
  let sorter = (\x y -> compare (snd x) (snd y))
  in sortBy sorter $ mapMaybe (hammingForKS content chunkCount) [lower..upper]

hammingForKS :: B.ByteString -> Int -> Int -> Maybe (Int, Float)
hammingForKS input chunkCount keySize =
  if B.length input < keySize * chunkCount
    then Nothing
    else Just (keySize, average hamming / fromIntegral keySize)
  where
    -- create chunks
    chunks = sizedChunksTake keySize chunkCount input
    -- create permutations
    subSeqs = subsequencesOfSize 2 chunks
    -- compute hamming
    hamming = fmap (\x -> hammingDistance (x !! 0) (x !! 1)) subSeqs
    average :: [Int] -> Float
    average it = fromIntegral (sum it) / fromIntegral chunkCount
