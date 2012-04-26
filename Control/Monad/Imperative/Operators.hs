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

(+=:), (*=:), (-=:) :: (ValTp k, Num b) => V Var r b -> V k r b -> MIO r ()
(+=:) a b = modifyOp (+) a b
(*=:) a b = modifyOp (*) a b
(-=:) a b = modifyOp (-) a b

(%=:) :: (ValTp k, Integral b) => V Var r b -> V k r b -> MIO r ()
(%=:) a b = modifyOp mod a b

(<.), (>.), (<=.),(<=.) :: (ValTp b1, ValTp b2, Ord b) => V b1 r b -> V b2 r b -> V Comp r Bool
(<.) a b = liftOp2 (<) a b
(>.) a b = liftOp2 (>) a b
(>=.) a b = liftOp2 (>=) a b
(<=.) a b = liftOp2 (<=) a b

(==.) :: (ValTp b1, ValTp b2, Eq b) => V b1 r b -> V b2 r b -> V Comp r Bool
(==.) a b = liftOp2 (==) a b

(+.), (-.), (*.) :: (ValTp b1, ValTp b2, Num c) => V b1 r c -> V b2 r c -> V Comp r c
(+.) a b = liftOp2 (+) a b
(-.) a b = liftOp2 (-) a b
(*.) a b = liftOp2 (*) a b

(%.) :: (ValTp b1, ValTp b2, Integral c) => V b1 r c -> V b2 r c -> V Comp r c
(%.) a b = liftOp2 mod a b

(/.) :: (ValTp b1, ValTp b2, Fractional c) => V b1 r c -> V b2 r c -> V Comp r c
(/.) a b = liftOp2 (/) a b 

(&&.), (||.) :: (ValTp b1, ValTp b2) => V b1 r Bool -> V b2 r Bool -> V Comp r Bool
(&&.) a b = liftOp2 (&&) a b
(||.) a b = liftOp2 (||) a b

(~.) :: ValTp b1 => V b1 r Bool -> V Comp r Bool
(~.) a = $(liftOp 'not) a

-- | @'liftOp2' f@ turns a pure function into one which
-- gets executes its arguments and returns their value as a 
-- function.  It is defined using 'liftOp'.
liftOp2 :: (ValTp b1, ValTp b2) => (a -> b -> c) -> V b1 r a -> V b2 r b -> V Comp r c
liftOp2 v = $(liftOp 'v)