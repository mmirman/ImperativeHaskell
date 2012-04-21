{-# LANGUAGE
 NoMonomorphismRestriction
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.ImperativeOperators
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  NoMonomorphismRestriction
-- Description :  Some predefined operators for the imperative monad.
-- License     :  GNUv3
-- 
-----------------------------------------------------------------------------
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
