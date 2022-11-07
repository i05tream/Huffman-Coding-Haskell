module Data.Huffman where

import Data.List (sort, sortOn)
import Data.Map.Strict (Map, fromList)
import Data.Ord (Down (Down))
import Data.Tree

data HuffmanTreeNode = SingleChar Char Int | Sum Int deriving (Show, Eq)

type HuffmanTree = Tree HuffmanTreeNode

{- |
 Huffman treeを作る際の最初の葉を求める関数

 葉は文字の出現頻度の降順で返される
 同じ出現頻度の文字の葉はASCII Codeの昇順に並べられる

 >>> leaves "BACAAB"
 [Node {rootLabel = SingleChar 'A' 3, subForest = []},Node {rootLabel = SingleChar 'B' 2, subForest = []},Node {rootLabel = SingleChar 'C' 1, subForest = []}]
-}
leaves :: String -> [HuffmanTree]
leaves cs = [Node (SingleChar c n) [] | (c, n) <- freqsDesc]
 where
  freqsDesc = sortOn (Down . snd) freqs
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

{- | 最も頻度の合計が小さくなるノードの組み合わせを選び、それらのノードを結んだ木を作成する
 最も頻度の合計が小さくなるノードの組み合わせが複数存在する場合、より左側の組み合わせが優先的に処理される
-}
combine :: [HuffmanTree] -> [HuffmanTree]
combine [] = []
combine [t] = [t]
combine ts =
  let freqs = map freq ts
      freqSums = zipWith (+) freqs (tail freqs)
      minSum = minimum freqSums
   in combine' minSum ts
 where
  combine' x (t : t' : rest) =
    if freq t + freq t' == x
      then Node (Sum x) [t, t'] : rest
      else t : combine' x (t' : rest)
  combine' _ [t] = [t]
  combine' _ [] = []

  freq (Node (SingleChar _ x) _) = x
  freq (Node (Sum x) _) = x

-- | 文字列からHuffman treeを生成する関数
huffmanTree :: String -> HuffmanTree
huffmanTree = leavesToTree . leaves
 where
  leavesToTree [t] = t
  leavesToTree ts = leavesToTree . combine $ ts

-- | 文字列から文字列ごとの符号のMapを生成する関数
huffmanCodeMap :: String -> Map Char String
huffmanCodeMap = fromList . table . huffmanTree
 where
  table t = table' t ""

  table' (Node (SingleChar c _) []) code = [(c, code)]
  table' (Node (Sum _) [left, right]) code =
    table' left (code ++ "0") ++ table' right (code ++ "1")
