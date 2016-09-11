module Interpreter where

import qualified Data.Map as DM
import Data.Maybe
import Data.List (intercalate, genericReplicate)
import Tokenizer

al :: [(String, String)]
al = [ ("If","if")
      ,("Else", "else")
      ,("And", "and")
  ]
mapFromAL :: DM.Map String String
mapFromAL = DM.fromList al

fixLetterCase :: String -> String
fixLetterCase s = fromMaybe s valueFromList 
  where
    --isInSet = DM.member s mapFromAL
    valueFromList = DM.lookup s mapFromAL


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
--
eol :: String
eol = "\n"

tab :: String
tab = " "

interpreter :: Statement -> String
interpreter (Seq li) = intercalate "\n" $ map interpreter li
interpreter (Indented level statement) = (intercalate "" $ genericReplicate level tab) ++ interpreter statement
interpreter (IfExpr expression) = "if " ++ expression 
interpreter (ElifExpr expression) = "elif " ++ expression 
interpreter (ElseExpr expression) = "else " ++ expression 
interpreter (ReturnExpr expression) = "return " ++ expression 
interpreter (PassExpr) = "pass"
interpreter (GivenComment s) = parseComment s
interpreter (Sentence s) = s 

parseComment :: Comment -> String
parseComment (LineComment s) = "#" ++ s ++ eol
parseComment (MultiLineComment s) = "\"\"\"" ++ s ++ "\"\"\""
