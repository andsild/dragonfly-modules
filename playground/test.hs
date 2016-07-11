{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import qualified Control.Applicative as CA
import Data.Char
import qualified Data.Map as DM
import Data.List
import Data.String.Utils
import Data.Functor.Identity
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token
import System.IO 
import System.Console.CmdArg

al :: [(String, String)]
al = [ ("If","if")
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
mainparser = stmtparser CA.<* eof
  where
      CmdArgs{..} <- CmdArgs $  
      stmtparser :: Parser Statement
      stmtparser = do { prespace <- many space
                      ; keyword <- stmtIdentifier
                      ; rest <- manyTill anyChar (try newline)
                        ; return (Keyword prespace keyword rest)
                    }
                    <|> return Nil

parsein fs     = concat `fmap` mapM readFile fs

data CmdArgs = File String deriving (Show)
main :: IO()
main = do
  CmdArgs{..} <- cmdArgs $
  print "okay"


-- autocmd BufWritePost test.hs silent exe 'silent ! (ghc -o test %  && test input.txt) 1> output.txt 2>&1'


