module Data.HuffmanSpec where

import Data.Huffman
import Data.Map.Strict as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "genFrequencyMap" $ do
    it "returns the Map of frequency of characters" $ do
      let result = Map.fromList [('A', 3), ('B', 2), ('C', 1)]
      genFrequencyMap "ABCABA" `shouldBe` result
  describe "orderByFrequency" $ do
    it "sort characters by frequency" $ do
      orderByFrequency "AAAABBBCCD" `shouldBe` "ABCD"
