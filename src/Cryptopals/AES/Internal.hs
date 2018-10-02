{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cryptopals.AES.Internal where

import qualified Data.ByteString as B

import           Data.Bits
import           Data.Word

newtype GF = GF { runGF :: Word8 } deriving (Eq, Show, Read, Bits)

-- TODO make as 8Bit -- just word

poly :: GF
--poly = GF 0xedb88320 -- x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1
poly = GF 0x1b -- x^8 + x^4 + x^3 + x + 1
--
--

-- | compute x * p(x)
xtimes :: GF -> GF
xtimes c = unsafeShiftL c 1 + if testBit c 7 then poly else 0

-- 0x57 + 0x83 = d4  -- in hex
-- 87   + 131  = 212 -- in decimal
--
-- 0x57 * 0x83 = c1  -- in hex
-- 87   * 131  = 193 -- in decimal

instance Num GF where
  (+) = xor
  (-) = xor
  _ * 0 = 0
  a * b = xtimes a * unsafeShiftR b 1 + if testBit b 0 then a else 0
  negate = id
  abs = id
  signum = fromIntegral . signum . runGF
  fromInteger i
    --  odd i = GF 0x80000000 -- x^0
    | odd i = GF 0x80 -- x^0
    | otherwise = GF 0

