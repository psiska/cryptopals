module Cryptopals.AES.InternalTest where

import Cryptopals.AES.Internal

import Data.Word

import Test.Tasty.Hspec


spec_GFComputationSpec :: Spec
spec_GFComputationSpec = do
  describe "polynomial computation" $ do
    it "0x57 + 0x83 should be 0xd4" $ do
      (GF 0x57) + (GF 0x83) `shouldBe` (GF 0xd4)
    it "0x57 * 0x83 should be 0xc1" $ do
      (GF 0x57) * (GF 0x83) `shouldBe` (GF 0xc1)
    it "0x57 * 0x13 should be 0xfe" $ do
      (GF 0x57) * (GF 0x13) `shouldBe` (GF 0xfe)
