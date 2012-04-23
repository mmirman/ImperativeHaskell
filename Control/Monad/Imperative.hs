-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  portable
-- A front end for the ImperativeMonad
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative (module X) where

import Control.Monad.Imperative.ImperativeMonad as X hiding (modifyOp, liftOp2)
import Control.Monad.Imperative.ImperativeOperators as X
