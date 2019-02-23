module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: Bool
prop_q = (result == True) && (mean == (10.01 + 10.05 + 9.92 + 10.01 + 9.97) / 5)

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 1000, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)
