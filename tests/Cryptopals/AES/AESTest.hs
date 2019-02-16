module Cryptopals.AES.AESTest where

import Cryptopals.AES.Internal
import Cryptopals.AES.AES
import Data.Word

import Test.Tasty.Hspec

testTemp :: [Word8]
testTemp = [0x09, 0xcf, 0x4f, 0x3c]


spec_rotWord :: Spec
spec_rotWord = do
  describe "rotWord" $ do
    it "rotates the word" $ do
      rotWord testTemp `shouldBe` [0xcf, 0x4f, 0x3c, 0x09]

spec_SubBytes :: Spec
spec_SubBytes = do
   describe "subWords" $ do
     it "" $ do
      subWord testTemp `shouldBe` [0x01, 0x8a, 0x84, 0xeb]

