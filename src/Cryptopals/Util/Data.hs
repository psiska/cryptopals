{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Cryptopals.Util.Data
  ( AnalysisResult(..)
  , Distance(..)
  , DecodeAnalysis(..)
  , DataAnalysis(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import           Data.Text.Prettyprint.Doc
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           Protolude

data AnalysisResult = AnalysisResult
  { inputData :: B.ByteString
  , keyUsed :: B.ByteString
  , resultData :: B.ByteString
  } deriving (Generic, Show)

data DataAnalysis = DataAnalysis
  { sourceData :: B.ByteString
  , results :: [DecodeAnalysis]
  } deriving (Generic, Show)

data DecodeAnalysis = DecodeAnalysis
  { key :: [Word8]
  , decrypted :: B.ByteString
  , validChar :: Int
  , charPercentage :: Double
  , distance :: Distance
  } deriving (Generic, Show)

data Distance = Distance
  { euclidean :: Double
  , manhattan :: Double
  , minkowski :: Double
  , cosine    :: Double
  , jaccard   :: Double
  } deriving (Generic, Show)


-- Pretty Printing functions
instance Pretty AnalysisResult where
  pretty AnalysisResult { keyUsed, resultData } =
    "-----------"
    <+> sep
    [ "key    :"
    , pretty $ TE.decodeUtf8 keyUsed
    , "result :"
    , pretty $ TE.decodeUtf8 resultData
    ]

instance Pretty DecodeAnalysis where
  pretty ea =
    "-----------"
    <+> hardline
    <+> sep genValues
    <+> hardline
    <+> pretty (distance ea)
    where
      genValues =
        [ "key  :"
        , pretty (key ea)
        , "validChars :"
        , pretty (validChar ea)
        , "from :"
        , pretty (B.length (decrypted ea))
        ]

instance Pretty Distance where
  pretty Distance { euclidean, manhattan, minkowski, cosine, jaccard } =
    vsep [ "euclidean" <+> ":" <+> pretty euclidean
         , "manhattan" <+> ":" <+> pretty manhattan
         , "minkowski" <+> ":" <+> pretty minkowski
         , "cosine   " <+> ":" <+> pretty cosine
         , "jaccard  " <+> ":" <+> pretty jaccard
         ]

instance Pretty DataAnalysis where
  pretty DataAnalysis { sourceData, results} =
    "source"
    <+> align (pretty (TE.decodeUtf8 sourceData))
    <+> hardline
    <+> vsep (map pretty results)
