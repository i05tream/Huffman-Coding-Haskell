module Data.HuffmanSpec where

import Data.Huffman
import Data.Map.Strict as Map
import Data.Tree (Tree (Node))
import Test.Hspec

spec :: Spec
spec = do
  describe "genFrequencyMap" $ do
    it "returns the Map of frequency of characters" $ do
      let result = Map.fromList [('A', 3), ('B', 2), ('C', 1)]
      genFrequencyMap "ABCABA" `shouldBe` result
  describe "initialFrequency" $ do
    it "returns List of leaves" $ do
      let result =
            [ (Node (SingleChar 'd' 3) [])
            , (Node (SingleChar 'e' 2) [])
            , (Node (SingleChar 'f' 1) [])
            ]
      initialFrequency "dedfed" `shouldBe` result
