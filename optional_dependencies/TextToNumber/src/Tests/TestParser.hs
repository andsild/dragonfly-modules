module Tests.TestParser where

import Parser
import Tokenizer

import           Test.HUnit
import           Text.Heredoc

import Data.String.Utils

test_parseNumberCombineTennerAndSingleDigit = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "forty") (GivenNumber (StringNumber "two") Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'forty two' as string"
              "42" output

test_parseNumberZero = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "zero") Nil
      output = rstrip $ interpreter tokenizedNumber 0

  assertEqual "parsing 'zero' as string"
              "0" output

-- Small number in front a big number
test_parseNumberCombineSingleWithHundred = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") Nil)
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'two 100 and ' as string"
              "200" output


test_parseWrittenAndNumericNumbers = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") (GivenNumber (StringNumber "sixty") (GivenNumber (IntegerNumber 4) Nil)))

      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'Two hundred and sixty 4' as string"
              "264" output

test_parseNumber = TestCase $ do
  let tokenizedNumber = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") (GivenNumber (StringNumber "sixty") (GivenNumber (StringNumber "four") Nil))) 
      output = interpreter tokenizedNumber 0

  assertEqual "parsing 'two hundred and sixty four' as string"
              "264" output


test_parseNumberAndWord = TestCase $ do
  let tokenizedSentence = GivenNumber (StringNumber "two") (NonNumericString "tigers" (NonNumericString "and" (GivenNumber (StringNumber "six") (NonNumericString "bears" Nil))))

  let output = rstrip $ interpreter tokenizedSentence 0
      in assertEqual "multiword and number" "2 tigers and 6 bears" output

tests :: Test
tests = TestList [
        TestLabel "test1" test_parseNumber
        , TestLabel "test1" test_parseNumberCombineSingleWithHundred
        , TestLabel "test1" test_parseNumberCombineTennerAndSingleDigit
        , TestLabel "test1" test_parseNumberAndWord
        , TestLabel "test1" test_parseWrittenAndNumericNumbers
        , TestLabel "test1" test_parseNumberZero
  ]
