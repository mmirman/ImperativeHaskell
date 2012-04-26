{-# LANGUAGE
 GADTs,
 EmptyDataDecls,
 GeneralizedNewtypeDeriving,
 MultiParamTypeClasses,
 FlexibleInstances
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.Internals
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  GADTs, EmptyDataDecls, GeneralizedNewtypeDeriving, 
--                MultiParamTypeClasses, FlexibleInstances
-- A module which defines the monad for ImperativeHaskell,  
-- and some control operator to interact with 'MIO'
-- 
-----------------------------------------------------------------------------
module Control.Monad.Imperative.Internals
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
       , V(Lit, C)
       , ValTp
       , MIO()
       , Comp
       , Val
       , Var
       , (=:)
       , (&)
       , val
       ) where

import Control.Monad.Cont
import Control.Monad.Reader
import Data.IORef

newtype MIO r a = MIO { getMIO :: ReaderT (Control r) (ContT r IO) a }
                deriving (Monad, MonadCont)

data Var
data Val
data Comp

class ValTp b where
instance ValTp Var where
instance ValTp Val where
instance ValTp Comp where

data Control r = InFunction (r -> ContT r IO ()) 
               | InLoop { controlBreak :: MIO r () 
                        , controlContinue :: MIO r () 
                        , controlReturn :: r -> MIO r ()
                        }

-- | @'returnF' value@ acts like the imperative return, where
-- if called, it will exit the current function and place the 
-- returned value into the current continuation.  Note, this
-- doesn't work inside of loops.  Inside of loops, we need
-- 'returnV'
returnF :: ValTp a => V a b b -> MIO b b
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
returnV :: ValTp a => V a b b -> MIO b ()
returnV a = returnF a >> return ()

class Returnable b r where
  -- | @'return''@ can act as returnF or returnV depending on use
  -- if it does not work, it is likely that type inference
  -- could not figure out a sensible alternative.
  return' :: ValTp a => V a b b -> MIO b r

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

-- | @'break''@ exists the current loop.
-- if called outside of a loop, rather than throwing a compilation error,
-- it will simply return a runtime error.
break' :: MIO a ()
break' = MIO ask >>= controlBreak

-- | 'continue'' continues the current loop, passing over
-- any control flow that is defined.
-- if called outside of a loop, rather than throwing a compilation error,
-- it will simply return a runtime error.
continue' :: MIO a ()
continue' = MIO ask >>= controlContinue



data V b r a where
  R :: IORef a -> V Var r a
  Lit :: a -> V Val r a
  C :: ValTp b => MIO r (V b r a) -> V Comp r a

val :: ValTp b => V b r a -> MIO r a
val v = case v of
  R r -> MIO $ liftIO $ readIORef r
  Lit v -> return v
  C m -> val =<< m

-- | @('&')a@ gets a reference/pointer to the variable specified
(&) :: V Var r a -> V Var s a
(&) (R a) = R a

-- | 'auto' should just be used where the 
-- type can be automatically infered and we don't need an initial value
-- Use caution, as it is simply an alternate name for 'undefined'
auto = undefined

-- | 'new' constructs a new reference to the specified pure value
new :: a -> MIO r (V Var r a)
new a = do
  r <- MIO $ liftIO $ newIORef a
  return $ R r


infixr 0 =:

-- | The 'Assignable' class is used to specify a value which can be 
-- computed imperatively.
class Assignable valt where 
  -- | @variable '=:' value@ executes @value@ and writes it  
  -- to the location pointed to by @variable@
  (=:) :: V Var r a -> valt r a -> MIO r ()
  
instance ValTp b => Assignable (V b) where  
  (=:) (R ar) br = MIO $ do
    b <- getMIO $ val br
    liftIO $ writeIORef ar b
    
instance Assignable MIO where  
  (=:) a br = do
    b <- br
    a =: Lit b

-- | @'for''(init, check, incr)@ acts like its imperative @for@ counterpart
for' :: ValTp b => (MIO r irr1, V b r Bool, MIO r irr2) -> MIO r () -> MIO r ()
for' (init, check, incr) body = init >> for_r
  where for_r = do
          do_comp <- val check
          when do_comp $ callCC $ \break_foo -> do
                         callCC $ \continue_foo -> MIO $ do
                           flip withReaderT (getMIO body) $ \inbod ->
                             InLoop (break_foo ()) (continue_foo ()) (controlReturn inbod)
                         incr
                         for_r

-- | @'while''(check)@ acts like its imperative @while@ counterpart.
while' :: ValTp b => V b r Bool -> MIO r () -> MIO r ()
while' check = for'(return (), check, return () )

-- | @'if''(check) act@ only performs @act@ if @check@ evaluates to true
-- it is specifically a value in its argument.
if' :: ValTp b => V b r Bool -> MIO r () -> MIO r ()
if' b m = do
  v <- val b
  when v m

-- | @'modifyOp'@ makes a modification assignment operator 
-- out of a binary haskell function.
-- The suggested use is to replicate functionality of assignments
-- like @-=@ or @%=@ from C style languages.
modifyOp :: ValTp k => (a->b->a) -> V Var r a -> V k r b -> MIO r ()
modifyOp op (R ar) br = MIO $ do
  b <- getMIO $ val br
  liftIO $ modifyIORef ar (\v -> op v b)