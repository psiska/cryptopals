{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge6 where

import           Control.Exception (bracket)
import qualified Cryptopals.Util as U

import qualified Data.ByteString as B
import           System.IO (openFile, hClose)
import           Protolude

keySize' :: Int
keySize' = 2


-- challenge6 2 "./resources/s1c6.txt"
challenge6 ::  FilePath -> IO (Maybe U.TextAnalysis)
challenge6 fp = do
  content <- bracket
    (openFile fp ReadMode)
    hClose
    (\inh -> B.hGetContents inh)
  putText  "Content"
  putText $ show content
  -- TODO base 64 decrypt
  let keySizes = U.properKeySize (2, 40) content
  --let keySizes = U.findKeySize 2 40 content
  let choosenKeySize = maybe 2 fst (head keySizes)
  --let choosenKeySize = (fst . head) keySizes
  --putStrLn $ "Found possible keysize: " <> show choosenKeySize
  putText $ "Found possible keysize: " <> show choosenKeySize

  let dataAnalysis = U.solveXorKey choosenKeySize content
  --putStrLn $ "Data" <> show dataAnalysis
  putText $ "Data" <> show dataAnalysis

  --let keys = map U.keyFromAnalysis dataAnalysis
  --putStrLn $ "Keys" <> show keys
  --TODO show decrypted text with those keys
  --
  let analysisResults = map (\ta -> U.ta2ar ta content) dataAnalysis
  --putStrLn $ "Results: " <> show analysisResults
  putText $ "Results: " <> show analysisResults

  return $ head dataAnalysis

  --let allResult = U.repeatingXorResults choosenKeySize content
  --return allResult

  {--

  putStrLn $ "Found results: " <> show allResult
  let key = U.keyFromAnalysis allResult
  putStrLn $ "Found possible key: " <> show key

  -- TODO Create result
  let result = U.xorByteStrings key content
  return result
  --}
