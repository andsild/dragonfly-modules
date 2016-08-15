module Tokenizer where

import Control.Monad.State
import Data.Functor.Identity
import Text.Parsec hiding (State)
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data Whitespace = IndentBlock Statement | Statement
data Expr = Code String | Nil
data Comment = LineComment String | MultiLineComment String
data Statement = Indented Whitespace Statement | Keyword String String String | GenericLine String | GivenExpr Expr | If Statement | GivenComment Comment

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

def :: LanguageDef st
def = emptyDef{ --commentStart = "#"
              --, commentLine = "\"\"\""
                identStart =  letter <|> char '_'
              , identLetter = alphaNum <|> oneOf "_-.,"
              , reservedOpNames = []
              , reservedNames = pythonKeywords
              , caseSensitive   = False
              }

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser def


indentParser :: Parser Whitespace
indentParser = do {
          preBlock <- char ' '
          ; rest <- mainparser
          ; return (IndentBlock rest)
          } 
          <|> return Statement

--do { prespace <- many space
--                      ; keyword <- stmtIdentifier
--                      ; rest <- many anyChar
--                        ; return (Keyword prespace keyword rest)
--                    } 

sentenceParser :: Parser Statement
sentenceParser = do
  {
    string <- stmtIdentifier
    ; rest <- manyTill anyChar (try newline <* eof)
    ; return (GenericLine string)
  }

ifexpr :: Parser Statement
ifexpr = do
  {
    stmtReservedName "if"
    ; rest <- mainparser
    ; return (If rest)
  }

commentParser :: Parser Statement
commentParser = do
  {
    commentStart <- string "\"\"\""
    ; comment <- manyTill anyChar (try (string "\"\"\""))
    ; return  (GivenComment (MultiLineComment comment))
  }
  <|> do 
  {
    commentStart <- char '#'
    ; comment <- manyTill anyChar (try newline)
    ; return  (GivenComment (LineComment comment))
  }

exprparser :: Parser Expr
exprparser = return Nil


mainparser :: Parser Statement
mainparser = givenParser <* eof
  where
    givenParser = sentenceParser <|> ifexpr <|> commentParser
       <|> do {
          spaces <- indentParser
          ; return (Indented spaces (GivenExpr Nil))
        } <|> return (GivenExpr Nil)
