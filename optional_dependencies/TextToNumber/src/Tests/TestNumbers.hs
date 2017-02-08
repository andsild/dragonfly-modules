module Tests.TestNumbers where

import Numbers

import           Test.HUnit
import           Text.Heredoc

test_lookupNumberInList = TestCase $ do
  let number = "fortytwo"

  case elemIndex' number of
    Just n -> assertEqual "parsing 'forty two' as string" 42 n
    Nothing -> assertFailure "parse error"


test_lookupNumberOutsideOfList = TestCase $ do
  let number = "hundred"

  case elemIndex' number of
    Just n -> assertEqual "parsing hundred' as string" 100 n
    Nothing -> assertFailure "parse error"


test_lookupNumberZero = TestCase $ do
  let number = "zero"

  case elemIndex' number of
    Just n -> assertEqual "parsing 'zero' as string" 0 n
    Nothing -> assertFailure "parse error"


test_lookupNumberBigNumber = TestCase $ do
  let number = "million"

  case elemIndex' number of
    Just n -> assertEqual "parsing million as string" 1000000 n
    Nothing -> assertFailure "parse error"


tests :: Test
tests = TestList [
        TestLabel "test1" test_lookupNumberInList
        , TestLabel "test1" test_lookupNumberOutsideOfList
        , TestLabel "test1" test_lookupNumberBigNumber
        , TestLabel "test1" test_lookupNumberZero
  ]
