module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: Bool
prop_q = expr == True

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 1000, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)
