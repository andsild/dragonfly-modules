{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Data.Data
import qualified Data.Map as DM
import Data.Maybe
import Data.List (intercalate)
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token
import System.Console.CmdArgs (cmdArgs, (&=), summary, argPos) 

al :: [(String, String)]
al = [ ("If","if")
      ,("Else", "else")
  ]
mapFromAL :: DM.Map String String
mapFromAL = DM.fromList al

data Statement = Keyword String String String | Nil
def :: LanguageDef st
def = emptyDef{ identStart =  anyChar
              , identLetter = alphaNum <|> char '-'
              , reservedOpNames = ["and"]
              , reservedNames = []
              }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser def

stmtIdentifier :: ParsecT String u Identity String
stmtIdentifier = identifier lexer
stmtReservedOp :: String -> ParsecT String u Identity ()
stmtReservedOp = reservedOp lexer
stmtNatural :: ParsecT String u Identity Integer
stmtNatural    = natural lexer
stmtSemiSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
stmtSemiSep    = semiSep lexer
stmtWhitespace :: ParsecT String u Identity ()
stmtWhitespace = whiteSpace lexer


mainparser :: Parser Statement
mainparser = stmtparser 
  where
      stmtparser :: Parser Statement
      stmtparser = do { prespace <- many space
                      ; keyword <- stmtIdentifier
                      ; rest <- many anyChar
                        ; return (Keyword prespace keyword rest)
                    }
                    <|> return Nil

fixLetterCase :: String -> String
fixLetterCase s = fromMaybe s (DM.lookup s mapFromAL )

alignSpace :: Int
alignSpace = 4

alignNumber :: Int -> Int -> Int
alignNumber aligner i = aligner * (i `div` aligner)

fixWhiteSpace :: String -> String
fixWhiteSpace s = replicate (alignNumber alignSpace $ length s) ' '


replaceC :: Char -> Char -> String -> String
replaceC _ _ [] = []
replaceC a b (x:xs)
  | x == a    = b:replaceC a b xs
  | otherwise = x:replaceC a b xs

-- TODO: need to parse word by word
replaceW :: String -> String -> String -> String
replaceW a b s = intercalate "\n"  . map replaceW' $ lines s
  where replaceW' x | x == a    = b
                    | otherwise = x

-- data Statement = Keyword String String String | Nil
interpreter :: Statement -> String
interpreter (Keyword whitespace command rest ) = alignedSpace ++ parsedCommand ++ rest 
  where
    alignedSpace = fixWhiteSpace whitespace
    parsedCommand = fixLetterCase command  ++ " "
interpreter Nil = ""

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
  mapM_ parseInput $ lines (replaceW "\\" "£" input)

-- autocmd BufWritePost test.hs silent exe 'silent ! (ghc -o test %  && test input.txt) 1> output.txt 2>&1'
