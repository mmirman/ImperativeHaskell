{-# LANGUAGE
 GADTs,
 EmptyDataDecls 
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.ImperativeMonad
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
-- Description :  A module for Imperative haskell code.
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

module Control.Monad.Imperative.ImperativeMonad 
       ( modifyOp
       , if'
       , for 
       , break
       , continue
       , returnV
       , function 
       , auto
       , runImperative
       , liftOp2
       , prim
       , returnF
       , (=:)
       , (&)
       ) where

import Prelude hiding (break)
import Control.Monad.Cont
import Control.Monad.Reader
import Data.IORef

data Var
data Val
data Comp

data Control r = InFunction (r -> ContT r IO ())
               | InLoop { controlBreak::MIO r ()
                        , controlContinue::MIO r ()
                        , controlReturn:: r -> MIO r ()
                        }

returnF v = do
  v' <- val v
  a <- ask
  case a of
    InLoop _ _ ret -> ret v'
    InFunction ret -> lift $ ret v'
  return v'

runImperative :: MIO a a -> IO a
runImperative foo = runContT (callCC $ \ret -> runReaderT foo $ InFunction ret) return

function :: MIO a a -> MIO b a
function = liftIO . runImperative

break :: MIO a ()
break = do
  a <- ask
  case a of
    InLoop br _ _ -> br
    _ -> return ()

continue :: MIO a ()
continue = do
  a <- ask
  case a of
    InLoop _ con _ -> con
    _ -> return ()

type MIO r a = ReaderT (Control r) (ContT r IO) a

data V b r a where
  R :: IORef a -> V Var r a
  L :: a -> V Val r a
  C :: MIO r (V b r a) -> V Comp r a

returnV a = returnF a >> return ()

val :: V b r a -> MIO r a
val v = case v of
  R r -> liftIO $ readIORef r
  L v -> return v
  C m -> val =<< m

(&) :: V Var r a -> V Var s a
(&) (R a) = R a

auto :: a -> MIO r (V Var r a)
auto a = do
  r <- liftIO $ newIORef a
  return $ R r

prim :: a -> V Val r a
prim a = L a


infixr 0 =:

(=:) :: V Var r a -> V b r a -> MIO r ()
(=:) (R ar) br = do
  b <- val br
  liftIO $ writeIORef ar b

for :: (MIO r irr1, V b r Bool, MIO r irr2) -> MIO r () -> MIO r ()
for (init, check, incr) body = init >> for'
  where for' = do
          do_comp <- val check
          when do_comp $ callCC $ \break_foo -> do
                         callCC $ \continue_foo -> do
                           flip withReaderT body $ \inbod ->
                             InLoop (break_foo ()) (continue_foo ()) (controlReturn inbod)
                         incr
                         for'

if' :: V b r Bool -> MIO r () -> MIO r ()
if' b m = do
  v <- val b
  when v m


modifyOp :: (a->b->a) -> V Var r a -> V k r b -> MIO r ()
modifyOp op (R ar) br = do
  b <- val br
  liftIO $ modifyIORef ar (\v -> op v b)

liftOp2 foo ar br = C $ do
  a <- val ar
  b <- val br
  return $ prim $ foo a b
