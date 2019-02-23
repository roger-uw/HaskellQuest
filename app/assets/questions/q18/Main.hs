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
     fib 4
  ~> fib (4 - 1) + fib (4 - 2)
  ~> fib 3 + fib (4 - 2)
  ~> (fib (3 - 1) + fib (3 - 2)) + fib (4 - 2)
  ~> (fib 2 + fib (3 - 2)) + fib (4 - 2)
  ~> ((fib (2 - 1) + fib (2 - 2)) + fib (3 - 2)) + fib (4 - 2)
  ~> ((fib 1 + fib (2 - 2)) + fib (3 - 2)) + fib (4 - 2)
  ~> ((1 + fib (2 - 2)) + fib (3 - 2)) + fib (4 - 2)
  ~> ((1 + fib 0) + fib (3 - 2)) + fib (4 - 2)
  ~> ((1 + 0) + fib (3 - 2)) + fib (4 - 2)
  ~> (1 + fib (3 - 2)) + fib (4 - 2)
  ~> (1 + fib 1) + fib (4 - 2)
  ~> (1 + 1) + fib (4 - 2)
  ~> 2 + fib (4 - 2)
  ~> 2 + fib 2
  ~> 2 + (fib (2 - 1) + fib (2 - 2))
  ~> 2 + (fib 1 + fib (2 - 2))
  ~> 2 + (1 + fib (2 - 2))
  ~> 2 + (1 + fib 0)
  ~> 2 + (1 + 0)
  ~> 2 + 1
  ~> 3
  ]|]

