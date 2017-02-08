module Main 
  where

import qualified Tests.TestParser as TP
import qualified Tests.TestTokenizer as TT
import qualified Tests.TestNumbers as TN
import Test.HUnit

main :: IO Counts
main = do
  -- runTest[T]ext(to)[T]erminal
  runTestTT $ TestList [ 
    TP.tests
    , TT.tests
    , TN.tests
    ]

