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
  describe "genHuffmanTree" $ do
    context "when number of tree is 1" $ do
      it "returns tree" $ do
        let left = Node (SingleChar 'a' 2) []
            right = Node (SingleChar 'b' 1) []
            huffmanTree = Node (Sum 3) [right, left]
            trees = [huffmanTree]
        genHuffmanTree trees `shouldBe` huffmanTree
    context "when some trees exist" $
      it "combine trees with minimum frequency" $ do
        let trees = [leaf 'x' 700, leaf 'y' 7, leaf 'z' 70]
            huffmanTree =
              Node
                (Sum 777)
                [ leaf 'x' 700
                , Node
                    (Sum 77)
                    [ leaf 'z' 70
                    , leaf 'y' 7
                    ]
                ]
        genHuffmanTree trees `shouldBe` huffmanTree
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
