module Main where

import Prelude hiding (length)

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: [Int] -> Bool
prop_q l = myLength l == answer l
  where answer :: [Int] -> Int
        answer [] = 0
        answer (_ : xs) = 1 + answer xs

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 1000, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)

