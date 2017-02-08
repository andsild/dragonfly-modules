module Main where

import Parser
import Tokenizer

import Data.String.Utils

import Text.Parsec

main :: IO ()
main = do
  c <- getContents
  case parse mainparser "(stdin)" c of
          Left e -> do putStrLn "Error parsing input:"
                       print e
          Right r -> putStrLn $  rstrip $  interpreter r 0

