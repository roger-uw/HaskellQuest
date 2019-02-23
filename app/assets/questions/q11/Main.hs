{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Main where

import qualified Test.QuickCheck as QC

import qualified Test.QuickCheck.Monadic as QCM

import qualified GenJSONInfo as J

import qualified Language.Haskell.TH as TH

import qualified Language.Haskell.TH.Alpha as THA

prop_q :: QC.Property
prop_q = QCM.monadicIO $ do
  a <- QCM.run (TH.runQ answer)
  r <- QCM.run (TH.runQ result)
  c <- QCM.run (THA.expEqual a r)
  QCM.assert c

main :: IO ()
main = do
  r <- QC.quickCheckWithResult QC.stdArgs{QC.maxSuccess = 5, QC.chatty = False} (QC.within 2000 prop_q)
  putStrLn (J.genJSONInfo r)

infixr 0 ~>

(~>) = (:)

infixr 0 #>

(#>) l x = l : [x]

answer = [|[
     (\f a b -> f (a * a) b) (\x y -> x - y) 5 ((\x -> x + 1) 7)
  ~> (\x y -> x - y) (5 * 5) ((\x -> x + 1) 7)
  ~> (5 * 5) - ((\x -> x + 1) 7)
  ~> 25 - ((\x -> x + 1) 7)
  ~> 25 - (7 + 1)
  ~> 25 - 8
  #> 17
  ,
     (\a b -> a b) (\x -> x + 2) (5 + 3)
  ~> (\x -> x + 2) (5 + 3)
  ~> (5 + 3) + 2
  ~> 8 + 2
  #> 10
  ,
     (\a b -> a || b) (5 > 3) ((\x -> x + 2 >= 7) 1)
  ~> (5 > 3) || ((\x -> x + 2 >= 7) 1)
  ~> True || ((\x -> x + 2 >= 7) 1)
  ~> True
  ]|]

