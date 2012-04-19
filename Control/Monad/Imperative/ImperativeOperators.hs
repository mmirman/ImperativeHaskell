{-# LANGUAGE
 NoMonomorphismRestriction
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.ImperativeOperators
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
-- Description :  Some predefined operators for the imperative monad.
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
