module Main (main) where

import Data.Huffman (huffmanCode)
import Lib

main :: IO ()
main = do
  text <- getLine
  putStrLn $ huffmanCode text
