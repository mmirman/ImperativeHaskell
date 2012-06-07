{-# LANGUAGE
 DataKinds,
 RankNTypes,
 TemplateHaskell,
 NoMonomorphismRestriction
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
-- Description :  An example module for Control.Monad.Imperative
-- 
-- Some exampes of use.
-----------------------------------------------------------------------------
module Main where

import Control.Monad.Imperative
  
swap(r1,r2) = function $ do
{
    z <- new auto;
    z =: r1;
    r1 =: r2;
    r2 =: z;
    return' r1;
};

imperativeId(r1) = function $ do
{
  return' r1;
};

type NumLit = forall r a . Num a => V TyVal r a

deferer() = function $ do
{ 
  a <- new 0;
  b <- new 2;
  
  defer' a;
  
  defer' $ do {
    io $ putStrLn "hi1";
    io $ putStrLn "hi2";
  };
  
  defer' $ do {
    imperativeId(a);
    io $ putStrLn "hi3";
    return' b; 
  };
  
  return' a;
}

print' v = do 
  v' <- val v
  io $ print v'

factorial :: (Show r, Num r, Ord r) => () -> MIO i b r
factorial() = function $ do
{
    a <- new 0;
    n <- new 1;
    
    a =: (0 :: NumLit);
    
    for' ( a =: Lit 1 , a <. Lit 11 , a +=: Lit 1 ) $ do
    {
      
        b <- new 0;
        b =: a;
        
        defer' $ do {
           print' b;
        };
        
        n *=: a;
        if' ( a <. Lit 5)
            continue';
        
        if' ( a >. Lit 2) 
            break';
        
        return' a;
        
    }; 
    
    a =: imperativeId(a);
    
    swap( (&)n , (&)a);
    
    return' a;
};

main = do
  t <- runImperative $ deferer()
  putStrLn $ "Defer: " ++ show t
  
  j <- runImperative $ factorial()
  putStrLn $ "Factorial: " ++ show j
