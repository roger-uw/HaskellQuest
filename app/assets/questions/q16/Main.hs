module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

import Data.List (sort)

prop_q :: Int -> Bool
prop_q i = fullSort (triples testInput) == fullSort (answer testInput)
  where testInput :: Int
        testInput = if i == 0 then 2 else abs (i * 2)
        fullSort :: [[Int]] -> [[Int]]
        fullSort = sort . map sort
        answer :: Int -> [[Int]]
        answer r = [[r + s, r + t, r + s + t] | 
          s <- [1..(floor (fromIntegral r / sqrt 2))], 
          t <- [quot (r * r) (2 * s)], 
          mod (quot (r * r) 2) s == 0]
  
main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 1000, QC.chatty = False} (QC.within 5000 prop_q)
  putStrLn (J.genJSONInfo r)


