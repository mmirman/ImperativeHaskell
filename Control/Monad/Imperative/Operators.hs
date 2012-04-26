{-# LANGUAGE
 TemplateHaskell
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.Operators
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  TemplateHaskell 
-- Some predefined operators for the imperative monad.
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative.Operators where

import Control.Monad.Imperative.Internals
import Control.Monad.Imperative.FunctionFactory

(+=:) a b = modifyOp (+) a b
(*=:) a b = modifyOp (*) a b
(-=:) a b = modifyOp (-) a b
(%=:) a b = modifyOp mod a b

(<.) a b = liftOp2 (<) a b
(>.) a b = liftOp2 (>) a b
(+.) a b = liftOp2 (+) a b
(*.) a b = liftOp2 (*) a b
(%.) a b = liftOp2 mod a b
(/.) a b = liftOp2 (/) a b 

-- | @'liftOp2' f@ turns a pure function into one which
-- gets executes its arguments and returns their value as a 
-- function.  It is defined using 'liftOp'.
liftOp2 :: (a -> b -> c) -> V b1 r a -> V b2 r b -> V Comp r c
liftOp2 v = $(liftOp 'v)