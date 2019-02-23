module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: QC.NonNegative Int -> Bool
prop_q (QC.NonNegative n) = fibs !! n == answer !! n
  where answer :: [Integer]
        answer = 0 : 1 : zipWith (+) answer (tail answer)

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 500, QC.chatty = False, QC.maxSize = 1000} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)