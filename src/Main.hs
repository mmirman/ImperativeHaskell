{-# LANGUAGE
 GADTs
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
-- 
-- A test/example module for Control.Monad.Imperative

module Main where

import Prelude hiding (break)
import Control.Monad.Imperative

swap(r1, r2) = function $ do
{
    z <- auto undefined;
    z =: r1;
    r1 =: r2;
    r2 =: z;
};

factorial = function $ do
{
    a <- auto 0;
    n <- auto 1;
    for ( a =: prim 1 , a <. prim 11 , a +=: prim 1 ) $ do
    {
        n *=: a;
        if' ( a <. prim 7)
            continue;

        if' ( a >. prim 5)
            break;
    };

    swap( (&)n , (&)a);

    returnF n;
};

 
main = do
  t <- runImperative factorial
  putStrLn $ "Some Factorial: "++show t
