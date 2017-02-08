module Tests.TestTokenizer where

import Tokenizer

import           Test.HUnit
import           Text.Heredoc
import Text.Parsec


test_translateNumber = TestCase $ do
  let numberOne = "one"
  let expectedToken = GivenNumber (StringNumber "one") Nil

  case parse mainparser "testCase" numberOne of
    Left e -> assertFailure "error in parser"
    Right r -> assertEqual "aa" r expectedToken

test_tokenizeNonNumber = TestCase $ do
  let text = "a sentence"
  let expectedToken = (NonNumericString "a" (NonNumericString "sentence" Nil))

  case parse mainparser "testCase" text of
    Left e -> assertFailure "error in parser"
    Right r -> assertEqual "parsed non-numeric text:" r expectedToken

test_translateConjunction = TestCase $ do
  let numberTwoHundredAndSixtyFour = "two hundred and sixty four"
  let expectedToken = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") (GivenNumber (StringNumber "sixty") (GivenNumber (StringNumber "four") Nil))) 

  case parse mainparser "testCase" numberTwoHundredAndSixtyFour of
    Left e -> assertFailure "error in parser"
    Right r -> assertEqual "aa" expectedToken r

test_tokenizeWrittenAndNumericNumbers = TestCase $ do
  let number = "Two hundred and sixty 4"
  let expectedToken = GivenNumber (StringNumber "two") (GivenNumber (StringNumber "hundred") (GivenNumber (StringNumber "sixty") (GivenNumber (IntegerNumber 4) Nil)))


  case parse mainparser "testCase" number of
    Left e -> assertFailure "error in parser"
    Right r -> assertEqual ("parsing '" ++ number ++ "'") expectedToken r


test_translateWordsAndNumbers = TestCase $ do
  let numberTwoHundredAndSixtyFour = "one tiger and five bears"
  let expectedToken = GivenNumber (StringNumber "one") (NonNumericString "tiger" (NonNumericString "and" (GivenNumber (StringNumber "five") (NonNumericString "bears" Nil))))

  case parse mainparser "testCase" numberTwoHundredAndSixtyFour of
    Left e -> assertFailure "error in parser"
    Right r -> assertEqual "words and numbers" expectedToken r

tests :: Test
tests = TestList [
        TestLabel "test1" test_translateNumber
        , TestLabel "test1" test_translateConjunction
        , TestLabel "test1" test_tokenizeNonNumber
        , TestLabel "Test1" test_translateWordsAndNumbers
        , TestLabel "Test1" test_tokenizeWrittenAndNumericNumbers
  ]

