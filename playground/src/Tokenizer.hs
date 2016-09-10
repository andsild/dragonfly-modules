{-# LANGUAGE FlexibleContexts #-}
module Tokenizer where

import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Text.Parsec hiding (State)
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token
import Data.List (nub)

data Expression = Num Integer | Nil
data Comment = LineComment String | MultiLineComment String
data Statement = Indented | GivenExpr Expression | If Expression 
                 | GivenComment Comment | Seq [Statement]

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
stmtReservedName :: String -> ParsecT String u0 Identity ()
stmtReservedName = reserved lexer

pythonKeywords :: [String]
pythonKeywords = ["and", "if", "else", "elif", "pass", "return"]


multiLineCommentString :: String
multiLineCommentString = "\"\"\""

-- We're not adding the commentStart,End,Line because we need to keep it
-- If we add it to the languagedef, parsec will see it as whitespace
def :: LanguageDef st
def = emptyDef{ --commentStart = "\"\"\""
              --, commentEnd = "\"\"\""
              --, commentLine = "#"
              identStart =  letter <|> char '_'
              , identLetter = alphaNum <|> oneOf "_-.,"
              , reservedOpNames = []
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
    ; return (If content)
  } <?> "an if expression"

commentParser :: Parser Statement
commentParser = do
  {
    commentStart <- string multiLineCommentString
    ; comment <- manyTill anyChar (try (string "\"\"\"")) 
    ; return  (GivenComment (MultiLineComment comment))
  }
  <|> do 
  {
    commentStart <- char '#'
    ; comment <- manyTill anyChar newline
    ; return  (GivenComment (LineComment comment))
  }

exprParser :: Parser Expression
exprParser = do
  {
     ; d <- stmtNatural
     ; return (Num d)
  }

singleStatementParser = 
  --commentParser 
  -- <|>
  ifexpr 
        <|> do {
          blankSpace <- skipMany1 space
          ; return (Indented)
        } -- <|> return (GivenExpr Nil)
        <|> do {
            skipMany space
            ; newline
            ; return (Indented)
        }
      -- <* (try (string "EOF"))

mainparser :: Parser Statement
mainparser = stmtParser 
  where
    stmtParser = do
      list <- join <$> manyTill (sepEndBy singleStatementParser newline) eof
      ; return (Seq list)
      --; return $ if length list == 1 then head list else Seq list
      <|> do 
        {
          eof
          ; return Indented
        }
