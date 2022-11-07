module Data.Huffman where

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
