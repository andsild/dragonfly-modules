module Tokenizer where

import qualified Control.Applicative as CA
import qualified Numbers

import Data.Char (toLower)
import Data.List (map)
import qualified Data.Map as DM
import Data.Maybe
import Data.Functor.Identity
import Data.String.Utils

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token

data Stmt = GivenNumber UnitStringNumber Stmt | NonNumericString String Stmt | Nil
          deriving (Show)
data UnitStringNumber = StringNumber String  | IntegerNumber Int
          deriving (Show)

instance Eq Stmt where
  Nil == Nil = True
  GivenNumber usn1 stmt1 == GivenNumber usn2 stmt2 = usn1 == usn2 && stmt1 == stmt2
  NonNumericString w1 stmt1 == NonNumericString w2 stmt2 = w1 == w2 && stmt1 == stmt2
  Nil == GivenNumber _ _= False

instance Eq UnitStringNumber where
  IntegerNumber n1 == IntegerNumber n2 = n1 == n2
  StringNumber s1 == StringNumber s2 = s1 == s2
  StringNumber s == IntegerNumber n = show n == s
  _ == _ = False


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



mainparser :: Parser Stmt
mainparser = stmtWhitespace >> stmtparser CA.<* eof
  where
      stmtparser :: Parser Stmt
      stmtparser = do
        { num <- stmtNatural
        ; skipMany (stmtReservedOp "and")
        ; rest <- stmtparser
        ; let asInt = fromIntegral num
              in return (GivenNumber (IntegerNumber asInt) rest)
        }
        <|> do
          { num <- stmtIdentifier
          ; let numNoHyphen = numberWithoutHyphens num
                lookupKey = with toLower numNoHyphen
                lookedUpNum = Numbers.elemIndex' (lookupKey)

          ; case lookedUpNum of
              Just number -> do
                  skipMany (stmtReservedOp "and")
                  rest <- stmtparser
                  return (GivenNumber (StringNumber lookupKey) rest)
              Nothing -> do
                rest <- stmtparser
                return ((NonNumericString num ) rest)
        }
        <|> return Nil
      numberWithoutHyphens num = replace "-" "" num
      with = Data.List.map
