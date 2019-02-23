module Main where

import qualified Test.QuickCheck as QC

import qualified GenJSONInfo as J

prop_q :: Double -> Double -> Double -> Double -> Double -> Bool
prop_q pX pY cX cY r = isInCircle pX pY cX cY r == answer
  where answer = square r > square (pX - cX) + square (pY - cY)

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 5000, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)