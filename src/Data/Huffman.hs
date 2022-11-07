module Data.Huffman where

import Data.List (sort, sortOn)
import Data.Map.Strict as Map
import Data.Tree

data HuffmanTreeNode = SingleChar Char Int | Sum Int deriving (Show, Eq)

{- |
 Huffman treeを作る際の最初の葉を求める関数

 葉は文字の出現頻度の降順で返される

 >>> leaves "BACAAB"
 [Node {rootLabel = SingleChar 'A' 3, subForest = []},Node {rootLabel = SingleChar 'B' 2, subForest = []},Node {rootLabel = SingleChar 'C' 1, subForest = []}]
-}
leaves :: String -> [Tree HuffmanTreeNode]
leaves cs = [Node (SingleChar c n) [] | (c, n) <- freqsDesc]
 where
  freqsDesc = reverse . sortOn snd $ freqs
  freqs = [(head chunk, length chunk) | chunk <- chunkSameChars . sort $ cs]

{- |
 文字列を同じ文字の連続ごとに区切る関数

 >>> chunkSameChars "AAABBCC"
 ["AAA","BB","CC"]

 >>> chunkSameChars ""
 []
-}
chunkSameChars :: String -> [String]
chunkSameChars "" = []
chunkSameChars cs@(c : _) = let (chunk, rest) = span (== c) cs in chunk : chunkSameChars rest

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
huffmanCodeTable (Node (Sum _) [left, right]) code = huffmanCodeTable left (code ++ "0") ++ huffmanCodeTable right (code ++ "1")
