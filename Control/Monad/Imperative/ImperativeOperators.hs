{-# LANGUAGE
 NoMonomorphismRestriction
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.ImperativeOperators
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  NoMonomorphismRestriction
-- Some predefined operators for the imperative monad.
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative.ImperativeOperators where

import Control.Monad.Imperative.ImperativeMonad (modifyOp, liftOp2)


(+=:) = modifyOp (+)
(*=:) = modifyOp (*)
(-=:) = modifyOp (-)
(%=:) = modifyOp (mod)

(<.) = liftOp2 (<)
(>.) = liftOp2 (>)
(+.) = liftOp2 (+)
(*.) = liftOp2 (*)
