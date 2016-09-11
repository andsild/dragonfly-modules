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
  | AssignmentStatement String Expression
  | GivenExpr Expression 
  | If Expression 
  | IfOneline Expression Statement 
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

ifexpr :: Parser Statement
ifexpr = do
  {
    stmtReservedName "if"
    ; skipMany space
    ; content <- exprParser
    ; char ':'
    -- ; oneLiner <- try $ manyTill space newline <|> newline
    -- ; oneLiner <- try (try assignmentStatementParser <|> try commentParser)
    -- ; return $ if null oneLiner then (IfOneline content oneLiner) else (If content)
    ; return $  (If content)
  } <?> "an if expression"

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

assignmentStatementParser :: Parser Statement
assignmentStatementParser = do
  {
    variable <- stmtIdentifier
    ; stmtReservedOp "="
    ; rhs <- exprParser
    ; return (AssignmentStatement variable rhs)
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

singleStatementParser = 
  indentParser
   <|> commentParser 
   <|> assignmentStatementParser
   <|> ifexpr 
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
          ; return (GivenExpr Nil)
        }
