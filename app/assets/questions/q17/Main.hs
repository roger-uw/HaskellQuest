module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: Int -> Bool
prop_q i = isLeap testInput == answer testInput
  where testInput = abs i
        answer :: Int -> Bool
        answer y
          | mod y 4 /= 0 = False
          | mod y 100 /= 0 = True
          | mod y 400 /= 0 = False
          | otherwise = True

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 1000, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)

