module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: Bool
prop_q = (result == True) && 
  (var == (
    (10.01 - mean) ** 2 + 
    (10.05 - mean) ** 2 + 
    (9.92 - mean) ** 2 + 
    (10.01 - mean) ** 2 + 
    (9.97 - mean) ** 2) / 5)

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 1000, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)
