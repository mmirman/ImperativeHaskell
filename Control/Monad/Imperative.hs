-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  Many GHC only extensions
-- A front end for the ImperativeMonad
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative (module X) where

import Control.Monad.Imperative.Internals as X hiding (val, V(C))
import Control.Monad.Imperative.Operators as X hiding (liftOp2)
import Control.Monad.Imperative.FunctionFactory as X