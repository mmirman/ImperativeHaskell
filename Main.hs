{-# LANGUAGE
 GADTs
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
-- Description :  An example module for Control.Monad.Imperative
-- 
-- License     :  GNUv3
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
