{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Data.Data
import Text.Parsec
import System.Console.CmdArgs (cmdArgs, (&=), summary, argPos) 

import Tokenizer
import Parser
import Interpreter

parsein :: [FilePath] -> IO String
parsein fs     = concat `fmap` mapM readFile fs

data Options = Options {file :: FilePath} deriving (Data, Typeable, Show)

parseInput :: String -> IO ()
parseInput inline = 
  case parse mainparser " " inline of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> putStrLn $  interpreter r


main :: IO ()
main = do
  Options{..} <- cmdArgs $ Options { file = "teeest" &= argPos 0 } 
                &= summary "test"
  input <- parsein [file]
  mapM_ parseInput $ lines input

-- autocmd BufWritePost test.hs silent exe 'silent ! (ghc -o test %  && test input.txt) 1> output.txt 2>&1'
