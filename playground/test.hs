module Main where

import           Data.Char
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map              as DM
import           Data.Maybe
import           Data.String.Utils


main :: IO ()
main = do
  c <- getContents
  print c





-- autocmd BufWritePost *.hs ! (ghc -o test %  && cat input.txt | test) 1> output.txt 2>&1


