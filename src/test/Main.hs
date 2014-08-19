{-# LANGUAGE OverloadedStrings #-}

-- | Main test suite.

module Main where

import Control.Monad
import Data.Monoid
import TryHaskell

-- | Main entry point.
main :: IO ()
main =
  do assert (SuccessResult ("'a'","Char","'a'") mempty mempty)
            (run "'a'" mempty mempty)
     assert (SuccessResult ("123","Num a => a","123") [] mempty)
            (run "123" mempty mempty)
     assert (ErrorResult "Evaluation killed!")
            (run "sum [1..]" mempty mempty)
     assert (SuccessResult ("head []","a","") mempty mempty)
            (run "head []" mempty mempty)
     assert (SuccessResult ("id","a -> a","") mempty mempty)
            (run "id" mempty mempty)
     assert (SuccessResult ("putStrLn","String -> IO ()","") mempty mempty)
            (run "putStrLn" mempty mempty)
     assert (GetInputResult mempty mempty)
            (run "getLine" mempty mempty)
     assert (SuccessResult ("getLine","IO String","\"x\"") mempty mempty)
            (run "getLine" ["x"] mempty)
     assert (SuccessResult ("(*)","Num a => a -> a -> a","") mempty mempty)
            (run "(*)" mempty mempty)
     assert (ErrorResult "Evaluation killed!")
            (run "let r = r in r :: ()" mempty mempty)
  where
    run e i f =
      do putStrLn ("Running " ++ e)
         muevalOrType e i f
    assert e m =
      do v <- m
         unless (v == e)
                (error ("Assertion failed:\n\n" ++ show e ++ " == " ++ show v))
