{-# LANGUAGE
 NoMonomorphismRestriction,
 TemplateHaskell
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.Operators
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  NoMonomorphismRestriction, TemplateHaskell 
-- Some predefined operators for the imperative monad.
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative.Operators where

import Control.Monad.Imperative.Internals
import Control.Monad.Imperative.FunctionFactory

(+=:) = modifyOp (+)
(*=:) = modifyOp (*)
(-=:) = modifyOp (-)
(%=:) = modifyOp (mod)

(<.) = liftOp2 (<)
(>.) = liftOp2 (>)
(+.) = liftOp2 (+)
(*.) = liftOp2 (*)
(%.) = liftOp2 mod
(/.) = liftOp2 (/)

-- | @'liftOp2' f@ turns a pure function into one which
-- gets executes its arguments and returns their value as a 
-- function.  It is defined using 'liftOp'.
liftOp2 :: (a -> b -> c) -> V b1 r a -> V b2 r b -> V Comp r c
liftOp2 v = $(liftOp 'v)