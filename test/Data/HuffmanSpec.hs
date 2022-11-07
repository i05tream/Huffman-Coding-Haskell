module Data.HuffmanSpec where

import Data.Huffman
import Data.Map.Strict as Map
import Data.Tree (Tree (Node))
import Test.Hspec

spec :: Spec
spec = do
  describe "leaves" $ do
    it "returns Huffman tree leaves ordered by frequency desc" $ do
      leaves "dedfed"
        `shouldBe` [ leaf 'd' 3
                   , leaf 'e' 2
                   , leaf 'f' 1
                   ]
  describe "combine" $ do
    context "when apply to leaves" $ do
      it "combine 2 leaves with least frequencie" $ do
        combine [leaf 'x' 5, leaf 'y' 4, leaf 'z' 3]
          `shouldBe` [ leaf 'x' 5
                     , Node (Sum 7) [leaf 'y' 4, leaf 'z' 3]
                     ]
  describe "huffmanTree" $ do
    context "when number of tree is 1" $ do
      it "returns tree" $ do
        let tree = Node (Sum 3) [left, right]
            left = leaf 'a' 2
            right = leaf 'b' 1
        huffmanTree [tree] `shouldBe` tree
    context "when some trees exist" $
      it "combine trees" $ do
        let leaves' = [leaf 'x' 700, leaf 'y' 7, leaf 'z' 70]
            tree =
              Node
                (Sum 777)
                [ leaf 'x' 700
                , Node
                    (Sum 77)
                    [ leaf 'z' 70
                    , leaf 'y' 7
                    ]
                ]
        huffmanTree leaves' `shouldBe` tree
  describe "huffmanCodeTable" $ do
    context "when apply to leaf" $
      it "return huffman code" $ do
        huffmanCodeTable (leaf 'a' 100) "001" `shouldBe` [('a', "001")]
    context "when apply to node" $ do
      it "return huffman codes of leaves" $ do
        huffmanCodeTable (Node (Sum 6) [leaf 'a' 4, leaf 'b' 2]) "00" `shouldBe` [('a', "000"), ('b', "001")]
 where
  leaf :: Char -> Int -> Tree HuffmanTreeNode
  leaf c x = Node (SingleChar c x) []
