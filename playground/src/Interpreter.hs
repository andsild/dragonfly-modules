module Interpreter where

import qualified Data.Map as DM
import Data.Maybe
import Data.List (intercalate)
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
interpreter (Seq li) = unlines $ map interpreter li
interpreter (Indented) = "\t"
interpreter (If expression) = "if " ++ parseExpr expression ++ ":"
interpreter (GivenExpr e) = parseExpr e
interpreter (GivenComment s) = parseComment s

parseComment :: Comment -> String
parseComment (LineComment s) = "#" ++ s ++ "\n"
parseComment (MultiLineComment s) = "\"\"\"" ++ s ++ "\"\"\""

parseExpr :: Expression -> String
parseExpr Nil = ""
parseExpr (Num c)  = show c
