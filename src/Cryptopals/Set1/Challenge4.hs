{-# LANGUAGE OverloadedStrings #-}
module Cryptopals.Set1.Challenge4 where

import           Control.Exception (bracket)
import qualified Cryptopals.Util as U

import           Data.Char (ord)
import           Data.Maybe      (catMaybes)
import qualified Data.ByteString as B
import           System.IO

challenge4 :: FilePath -> IO ([U.XorFinding])
challenge4 fp = do
  content <- bracket
    (openFile fp ReadMode)
    hClose
    (\inh -> B.hGetContents inh)
  -- now analyse each and every line
  let lines' = B.split (fromIntegral (ord '\n')) content
  let opts = catMaybes $ map U.findSingleXor lines'
  return $ take 20 (U.sortOptions opts)

--, Just
--    ( 53
---    , 0.80695605
--    , 29
 --   , 96.666664
--    , "Now that the party is jumping"
--    , "7b5a4215415d544115415d5015455447414c155c46155f4058455c5b523f"
 --   , "{ZBA]TAA]PETGAL\F_@XE\[R?"
  --  ) <Paste>

