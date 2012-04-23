{-# LANGUAGE
 GADTs,
 EmptyDataDecls,
 GeneralizedNewtypeDeriving,
 MultiParamTypeClasses,
 FunctionalDependencies,
 FlexibleInstances,
 UndecidableInstances
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.ImperativeMonad
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  GADTs, EmptyDataDecls
-- License     :  GNU-3
-- Description :  A module for Imperative haskell code.
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative.ImperativeMonad 
       ( modifyOp
       , if'
       , for'
       , while'
       , break'
       , continue'
       , return'
       , returnV
       , returnF
       , function 
       , new
       , auto
       , runImperative
       , liftOp
       , liftOp2
       , liftOp3         
       , liftOp4         
       , liftOp5
       , V(Lit)
       , returnF
       , (=:)
       , (&)
       ) where

import Control.Monad.Cont
import Control.Monad.Reader
import Data.IORef

newtype MIO r a = MIO { getMIO :: ReaderT (Control r) (ContT r IO) a }
                deriving (Monad, MonadCont)

data Var
data Val
data Comp

data Control r = InFunction (r -> ContT r IO ())
               | InLoop { controlBreak::MIO r ()
                        , controlContinue::MIO r ()
                        , controlReturn:: r -> MIO r ()
                        }

-- | @'returnF' value@ acts like the imperative return, where
-- if called, it will exit the current function and place the 
-- returned value into the current continuation.  Note, this
-- doesn't work inside of loops.  Inside of loops, we need
-- 'returnV'
returnF :: V a b b -> MIO b b
returnF v = MIO $ do
  v' <- getMIO $ val v
  a <- ask
  case a of
    InLoop _ _ ret -> getMIO $ ret v'
    InFunction ret -> lift $ ret v'
  return v'
  
-- | @'returnV' value@ acts like the imperative return, where
-- if called, it will exit the current function and place the 
-- returned value into the current continuation.  Note, this
-- doesn't work as a last function call.
returnV :: V a b b -> MIO b ()
returnV a = returnF a >> return ()

class Returnable b r where
  -- | @'return''@ can act as returnF or returnV depending on use
  -- if it does not work, it is likely that type inference
  -- could not figure out a sensible alternative.
  return' :: V a b b -> MIO b r

instance Returnable b () where
  return' a = returnV a 

instance Returnable b b where
  return' a = returnF a


runImperative :: MIO a a -> IO a
runImperative foo = do
  a <- runContT (callCC $ \ret -> runReaderT (getMIO foo) $ InFunction ret) return
  return a

-- | @'function' foo@ takes an ImperativeMonad action and removes it from it's  
-- specific function context, specifically making it applicable 
-- in the body of other functions.
function :: MIO a a -> MIO b a
function = MIO . liftIO . runImperative

-- | @'break'@ exists the current loop.
break' :: MIO a ()
break' = do
  a <- MIO $ ask
  case a of
    InLoop br _ _ -> br
    _ -> return ()
    
-- | @'continue'@ continues the current loop, passing over
-- any control flow that is defined.
continue' :: MIO a ()
continue' = do
  a <- MIO $ ask
  case a of
    InLoop _ con _ -> con
    _ -> return ()

data V b r a where
  R :: IORef a -> V Var r a
  Lit :: a -> V Val r a
  C :: MIO r (V b r a) -> V Comp r a


val :: V b r a -> MIO r a
val v = case v of
  R r -> MIO $ liftIO $ readIORef r
  Lit v -> return v
  C m -> val =<< m

-- | @('&')a@ gets a reference/pointer to the variable specified
(&) :: V Var r a -> V Var s a
(&) (R a) = R a

-- | @auto@ should just be used where the 
-- type can be automatically infered and we don't need an initial value
auto = undefined

-- | @new@ constructs a new reference object with the value specified
new :: a -> MIO r (V Var r a)
new a = do
  r <- MIO $ liftIO $ newIORef a
  return $ R r

infixr 0 =:


class Assignable val where 
  -- | @var '=:' value@ simply rewrites whatever 
  -- is in @var@ with whatever @value@ is.
  (=:) :: V Var r a -> val r a -> MIO r ()
  
instance Assignable (V b) where  
  (=:) (R ar) br = MIO $ do
    b <- getMIO $ val br
    liftIO $ writeIORef ar b
    
instance Assignable MIO where  
  (=:) a br = do
    b <- br
    a =: Lit b

-- | @'for'(init, check, incr)@ acts like the usual imperative for loop
for' :: (MIO r irr1, V b r Bool, MIO r irr2) -> MIO r () -> MIO r ()
for' (init, check, incr) body = init >> for_r
  where for_r = do
          do_comp <- val check
          when do_comp $ callCC $ \break_foo -> do
                         callCC $ \continue_foo -> MIO $ do
                           flip withReaderT (getMIO body) $ \inbod ->
                             InLoop (break_foo ()) (continue_foo ()) (controlReturn inbod)
                         incr
                         for_r

-- | @'while'(check)@ acts like the usual imperative while
while' :: V b r Bool -> MIO r () -> MIO r ()                         
while' check = for'(return (), check, return () )

-- | @'if'(check) m@ only executes m if the check is true.
-- it is specifically value in it's argument.
if' :: V b r Bool -> MIO r () -> MIO r ()
if' b m = do
  v <- val b
  when v m

-- | @'modifyOp' f@ makes a modify operator out of a binary 
-- haskell function
modifyOp :: (a->b->a) -> V Var r a -> V k r b -> MIO r ()
modifyOp op (R ar) br = MIO $ do
  b <- getMIO $ val br
  liftIO $ modifyIORef ar (\v -> op v b)

-- | @'liftOp' f@ turns a pure function into one which
-- gets the values out of it's arguments
liftOp foo a = C $ do
  a' <- val a
  return $ Lit $ foo a'

liftOp2 foo a1 a2 = C $ do
  a1' <- val a1
  a2' <- val a2
  return $ Lit $ foo a1' a2'

liftOp3 foo v1 v2 v3 = C $ do
  v1' <- val v1
  v2' <- val v2
  v3' <- val v3
  return $ Lit $ foo v1' v2' v3'
  
liftOp4 foo v1 v2 v3 v4 = C $ do
  v1' <- val v1
  v2' <- val v2
  v3' <- val v3
  v4' <- val v4
  return $ Lit $ foo v1' v2' v3' v4'
  
liftOp5 foo v1 v2 v3 v4 v5 = C $ do
    v1' <- val v1
    v2' <- val v2
    v3' <- val v3
    v4' <- val v4
    v5' <- val v5  
    return $ Lit $ foo v1' v2' v3' v5' v4'  
