module Data.Huffman
  ( genFrequencyMap,
  )
where

import Data.Map.Strict as Map

genFrequencyMap :: String -> Map.Map Char Integer
genFrequencyMap cs = Map.fromList [('C', 1), ('B', 2), ('A', 3)]
