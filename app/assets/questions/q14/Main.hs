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

infixr 0 ~~~>

(~~~>) = (:)

infixr 0 #>

(#>) l x = l : [x]

answer = [|[
     plus one two (\n -> n + 1) 0
  ~> (\f x -> one f (two f x)) (\n -> n + 1) 0
  ~> one (\n -> n + 1) (two (\n -> n + 1) 0)
  ~> (\n -> n + 1) (two (\n -> n + 1) 0)
  ~> (two (\n -> n + 1) 0) + 1
  ~> ((\n -> n + 1) ((\n -> n + 1) 0)) + 1
  ~~~> ((0 + 1) + 1) + 1
  #> 3
  ,
     mult two three (\n -> n + 1) 0
  ~> (\f x -> two (\z -> three f z) x) (\n -> n + 1) 0
  ~> two (\z -> three (\n -> n + 1) z) 0
  ~> (\z -> three (\n -> n + 1) z) ((\z -> three (\n -> n + 1) z) 0)
  ~> three (\n -> n + 1) ((\z -> three (\n -> n + 1) z) 0)
  ~> (\n -> n + 1) ((\n -> n + 1) ((\n -> n + 1) ((\z -> three (\n -> n + 1) z) 0)))
  ~> ((\n -> n + 1) ((\n -> n + 1) ((\z -> three (\n -> n + 1) z) 0))) + 1
  ~~~> ((((\z -> three (\n -> n + 1) z) 0) + 1) + 1) + 1
  ~> (((three (\n -> n + 1) 0) + 1) + 1) + 1
  ~> ((((\n -> n + 1) ((\n -> n + 1) ((\n -> n + 1) 0))) + 1) + 1) + 1
  ~~~> (((((0 + 1) + 1) + 1) + 1) + 1) + 1
  #> 6
  ]|]

