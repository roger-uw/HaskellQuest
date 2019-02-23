module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: Double -> Double -> Double -> Bool
prop_q a b c = hasRoot a b c == answer
  where answer = square b - 4 * a * c >= 0

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 5000, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)