{-# LANGUAGE FlexibleContexts #-}
module Tokenizer where

import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.List (nub, genericReplicate)
import Text.Parsec hiding (State)
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Debug.Trace

data Expression = Rhs String | Nil deriving Show
data Comment = LineComment String | MultiLineComment String deriving Show
data Statement = Indented Integer
  | Sentence String
  | IfExpr String
  | ElifExpr String
  | ElseExpr String
  | PassExpr 
  | ReturnExpr String
  | GivenComment Comment 
  | Seq [Statement]
  deriving Show

stmtIdentifier :: ParsecT String u Identity String
stmtIdentifier = identifier lexer
stmtReservedOp :: String -> ParsecT String u Identity ()
stmtReservedOp = reservedOp lexer
stmtNatural :: ParsecT String u Identity Integer
stmtNatural    = natural lexer
stmtSemiSep :: ParsecT String u Identity a -> ParsecT String u Identity [a]
stmtSemiSep    = semiSep lexer
stmtReservedName :: String -> ParsecT String u0 Identity ()
stmtReservedName = reserved lexer

pythonKeywords :: [String]
pythonKeywords = ["and", "if", "else", "elif", "pass", "return"]

multiLineCommentString :: String
multiLineCommentString = "\"\"\""

stmtReservedOpNames :: [String]
stmtReservedOpNames = ["="]

-- We're not adding the commentStart,End,Line because we need to keep it
-- If we add it to the languagedef, parsec will see it as whitespace
def :: LanguageDef st
def = emptyDef{ --commentStart = "\"\"\""
              --, commentEnd = "\"\"\""
              --, commentLine = "#"
              identStart =  letter <|> char '_'
              , identLetter = alphaNum <|> oneOf "_-."
              , reservedOpNames = stmtReservedOpNames
              , reservedNames = pythonKeywords
              , caseSensitive   = False
              }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser def

commentParser :: Parser Statement
commentParser = do
  {
    commentStart <- string multiLineCommentString
    ; comment <- manyTill anyChar (string multiLineCommentString)
    ; return  (GivenComment (MultiLineComment comment))
  }
  <|> do 
  {
    commentStart <- char '#'
    ; comment <- manyTill anyChar (lookAhead newline)
    ; return  (GivenComment (LineComment comment))
  }

exprParser :: Parser Expression
exprParser = do
  {
     ; d <- many alphaNum
     ; return (Rhs d)
  }

alignSpace :: Integer
alignSpace = 4

alignNumber :: Integer -> Integer -> Integer
alignNumber aligner i = aligner * (i `div` aligner)

fixWhiteSpace :: String -> String
fixWhiteSpace s = genericReplicate (alignNumber alignSpace $ toInteger $ length s) ' '

indentParser :: Parser Statement
indentParser = do
  {
    numSpaces <- many1 space
    ; return (Indented (toInteger $ alignNumber alignSpace $ toInteger $ length numSpaces))
  }

sentenceParser :: Parser Statement
sentenceParser = do
  {
    sentence <- manyTill (satisfy (\x -> not (x == '\n'))) newline
    ; return (Sentence sentence)
  }


untilNewline :: Stream s m Char => ParsecT s u m String
untilNewline = manyTill (satisfy (\x -> not (x == '\n'))) newline

keywordParser :: Parser Statement
keywordParser = do
  {
    stmtReservedOp "if"
    ; rest <- untilNewline
    ; return (IfExpr rest)
  }
  <|> do
  {
    stmtReservedOp "elif"
    ; rest <- untilNewline
    ; return (ElifExpr rest)
  }
  <|> do
  {
    stmtReservedOp "else"
    ; rest <- untilNewline
    ; return (ElseExpr rest)
  }
  <|> do
  {
    stmtReservedOp "pass"
    ; return (PassExpr)
  }
  <|> do
  {
    stmtReservedOp "return" 
    ; rest <- untilNewline
    ; return (ReturnExpr rest)
  }


singleStatementParser = 
  indentParser
   <|> sentenceParser
   <|> keywordParser
   <|> commentParser 
      -- <* (try (string "EOF"))

mainparser :: Parser Statement
mainparser = stmtParser 
  where
    stmtParser = do
    {
      list <- join <$> manyTill (sepEndBy singleStatementParser newline) eof
      ; return $ trace (show list) $ if length list == 1 then  head list else Seq list
    }
      <|> do 
        {
          eof
          ; return (Sentence "")
        }
