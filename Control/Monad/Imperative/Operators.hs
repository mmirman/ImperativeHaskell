{-# LANGUAGE
 NoMonomorphismRestriction,
 DataKinds,
 TypeFamilies,
 FlexibleContexts
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.Operators
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  NoMonomorphismRestriction
-- Some predefined operators for the imperative monad.
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative.Operators where

import Control.Monad.Imperative.Internals

(+=:), (*=:), (-=:) :: (HasValue (V k) i, Num b) => V TyVar r b -> V k r b -> MIO i r ()
(+=:) a b = modifyOp (+) a b
(*=:) a b = modifyOp (*) a b
(-=:) a b = modifyOp (-) a b

(%=:) :: (HasValue (V k) i, Integral b) => V TyVar r b -> V k r b -> MIO i r ()
(%=:) a b = modifyOp mod a b

(<.), (>.), (>=.), (<=.) :: (Ord c, HasValue (V b1) i, HasValue (V b2) i) => V b1 r c -> V b2 r c -> V (TyComp i TyVal) r Bool
(<.) a b = liftOp2 (<) a b
(>.) a b = liftOp2 (>) a b
(>=.) a b = liftOp2 (>=) a b
(<=.) a b = liftOp2 (<=) a b

(==.) :: (Eq c, HasValue (V b1) i, HasValue (V b2) i) => V b1 r c -> V b2 r c -> V (TyComp i TyVal) r Bool
(==.) a b = liftOp2 (==) a b

(+.), (-.), (*.) :: (Num c, HasValue (V b1) i, HasValue (V b2) i) => V b1 r c -> V b2 r c -> V (TyComp i TyVal) r c
(+.) a b = liftOp2 (+) a b
(-.) a b = liftOp2 (-) a b
(*.) a b = liftOp2 (*) a b

(%.) :: (Integral c, HasValue (V b1) i, HasValue (V b2) i) => V b1 r c -> V b2 r c -> V (TyComp i 'TyVal) r c
(%.) a b = liftOp2 mod a b

(/.) :: (State i, HasValue (V b1) i, HasValue (V b2) i, Fractional c) => V b1 r c -> V b2 r c -> V (TyComp i TyVal) r c
(/.) a b = liftOp2 (/) a b 

(&&.), (||.) :: (State i, HasValue (V b1) i , HasValue (V b2) i) => V b1 r Bool -> V b2 r Bool -> V (TyComp i TyVal) r Bool
(&&.) a b = liftOp2 (&&) a b
(||.) a b = liftOp2 (||) a b

(~.) :: (State i, HasValue (V b) i) => V b r Bool -> V (TyComp i TyVal) r Bool
(~.) a = C $ do 
  a' <- val a
  return $ Lit $ not a'

-- | @'liftOp2' f@ turns a pure function into one which
-- gets executes its arguments and returns their value as a
-- function.  It is defined using 'liftOp'.
liftOp2 :: (HasValue (V b1) i, HasValue (V b2) i) => (a -> b -> c) -> V b1 r a -> V b2 r b -> V (TyComp i TyVal) r c
liftOp2 f v1 v2 = C $ do
  v1' <- val v1
  v2' <- val v2
  return $ Lit $ f v1' v2'