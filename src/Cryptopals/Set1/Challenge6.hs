{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge6 where

import qualified Cryptopals.Util as U

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Text.Prettyprint.Doc
import           Protolude

keySize' :: Int
keySize' = 2


-- challenge6 2 "./resources/s1c6.txt"
challenge6 ::  FilePath -> IO ()
challenge6 fp = do
  content <- U.stripSpace <$> B.readFile fp
  putText  "Raw content:"
  putText $ show content

  case B64.decode content of
    Left err -> putText $ "Base64 decode failed - err: " <> T.pack err
    Right inputContent -> challenge6Internal inputContent


challenge6Internal :: B.ByteString -> IO ()
challenge6Internal inputContent = do
  putText "Base64 decoded input:"
  putText $ show inputContent

  let keySizes = U.findKeySize (2, 40) 4 inputContent
  putText $ "Top five keysizes found: " <> show (take 5 keySizes)
  let choosenKeySize = maybe 2 fst (head keySizes)
  putText $ "Found possible keysize: " <> show choosenKeySize

  let dataAnalysis = U.solveXorKey choosenKeySize inputContent

  --putText "Data:"
  --putText $ show $ pretty dataAnalysis

  let computedKeys = U.keysFromData dataAnalysis
  putText "ComputedKeys - raw: "
  putText $ show computedKeys
  putText "ComputedKeys - utf8 encoded: "
  putText $ show (map (TE.decodeUtf8 . B.pack) computedKeys)

  let analysisResults = map (U.applyKey inputContent) computedKeys
  putText "Results: "
  putText $ show $ pretty $ head analysisResults
