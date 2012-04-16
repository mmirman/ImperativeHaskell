{-# LANGUAGE
 NoMonomorphismRestriction
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.ImperativeOperators
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Some predefined operators for the imperative monad.

module Control.Monad.Imperative.ImperativeOperators where

import Control.Monad.Imperative.ImperativeMonad


(+=:) = modifyOp (+)
(*=:) = modifyOp (*)
(-=:) = modifyOp (-)
(%=:) = modifyOp (mod)

(<.) = liftOp2 (<)
(>.) = liftOp2 (>)
(+.) = liftOp2 (+)
(*.) = liftOp2 (*)
