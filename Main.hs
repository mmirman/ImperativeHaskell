{-# LANGUAGE
 GADTs,
 TemplateHaskell,
 MultiParamTypeClasses,
 DataKinds,
 RankNTypes,
 TypeFamilies
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
-- Description :  An example module for Control.Monad.Imperative
-- 
-- Copyright (C) 2012  Matthew Mirman
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>

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

factorial() = function $ do
{
    a <- new 0;
    n <- new 1;
    
    a =: (0 :: NumLit);

    for' ( a =: Lit 1 , a <. Lit 11 , a +=: Lit 1 ) $ do
    {
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
  t <- runImperative $ factorial()
  putStrLn $ "Some Factorial: " ++ show t
