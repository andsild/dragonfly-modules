module Parser where

import qualified Numbers
import Tokenizer

import qualified Data.Map as DM
import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils


interpreter :: Stmt -> Int -> String
interpreter (GivenNumber number stmt) acc
  | parsedNumber == 0 && acc == 0 = "0 " ++ interpreter stmt 0
  | parsedNumber == 0 && not (acc == 0) = show acc ++ " 0 " ++ interpreter stmt 0
  | acc == 0 = interpreter stmt $ parsedNumber
  | otherwise = interpreter stmt $ calculateNumber parsedNumber acc
  where
    parsedNumber = translate number
interpreter (NonNumericString word stmt) acc
  | acc == 0 = word ++ " " ++ interpreter stmt 0
  | otherwise = show acc ++ " " ++ word ++ " " ++ interpreter stmt 0
interpreter Nil acc
  | acc == 0 = ""
  | otherwise = show acc

calculateNumber :: Int -> Int -> Int
calculateNumber x prev
  | prev > 0 && prev <  x =  prev * x
  | prev > 0 && prev >  x =  prev + x
  | otherwise = x

translate :: UnitStringNumber -> Int
translate (IntegerNumber num) = num
translate (StringNumber num) = fromJust $ Numbers.elemIndex' num
