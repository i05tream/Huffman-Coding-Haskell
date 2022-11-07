module Data.Huffman where

import Data.List (sortOn)
import Data.Map.Strict as Map
import Data.Tree

data HuffmanTreeNode = SingleChar Char Int | Sum Int deriving (Show, Eq)

genFrequencyMap :: String -> Map.Map Char Int
genFrequencyMap cs =
  let cs' = zip cs $ repeat 1
   in Map.fromListWith (+) cs'

initialFrequency :: String -> [Tree HuffmanTreeNode]
initialFrequency cs =
  let freqs = genFrequencyMap cs
   in Map.elems . Map.mapWithKey (\k v -> Node (SingleChar k v) []) $ freqs

genHuffmanTree :: [Tree HuffmanTreeNode] -> Tree HuffmanTreeNode
genHuffmanTree [t] = t
genHuffmanTree ts =
  let sortedByFreq = sortOn extractFreq ts
      (minTree : nextMinTree : restTrees) = sortedByFreq
   in genHuffmanTree $ combine minTree nextMinTree : restTrees
 where
  extractFreq :: Tree HuffmanTreeNode -> Int
  extractFreq (Node (SingleChar _ x) _) = x
  extractFreq (Node (Sum x) _) = x

  combine :: Tree HuffmanTreeNode -> Tree HuffmanTreeNode -> Tree HuffmanTreeNode
  combine less greater =
    Node (Sum (extractFreq less + extractFreq greater)) [greater, less]

huffmanCodeTable :: Tree HuffmanTreeNode -> String -> [(Char, String)]
huffmanCodeTable (Node (SingleChar c _) []) code = [(c, code)]
