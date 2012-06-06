{-# LANGUAGE
 GADTs,
 EmptyDataDecls,
 GeneralizedNewtypeDeriving,
 MultiParamTypeClasses,
 FlexibleInstances,
 ExistentialQuantification,
 FlexibleContexts,
 UndecidableInstances,
 TypeFamilies,
 ScopedTypeVariables,
 FunctionalDependencies 
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
       , function 
       , new
       , auto
       , runImperative
       , V(Lit, C)       
       , MIO()
       , Comp
       , Val
       , Var
       , (=:)
       , (&)
       , val
       )  where

import Control.Monad.Cont
import Control.Monad.Reader
import Data.IORef
import Data.String (IsString(..))

data InLoop
data InFunction

data Var
data Val
data Comp i b

newtype MIO i r a = MIO { getMIO :: ReaderT (Control i r) (ContT r IO) a }
                  deriving (Monad, MonadCont, MonadIO)
  
data V b r a where
  R :: IORef a -> V Var r a
  Lit :: a -> V Val r a
  C :: MIO i r (V b r a) -> V (Comp i b) r a
  
data Control i r where  
  InFunction :: (r -> ContT r IO ()) -> Control InFunction r
  InLoop :: MIO InLoop r () -> MIO InLoop r () -> (r -> ContT r IO ()) -> Control i r

getReturn :: Control i r -> r -> ContT r IO ()
getReturn (InFunction ret) = ret
getReturn (InLoop _ _ ret) = ret

mioLoop :: ReaderT (Control InLoop r) (ContT r IO) a -> MIO InLoop r a
mioLoop = MIO 

mioFunc :: ReaderT (Control InFunction r) (ContT r IO) a -> MIO InFunction r a
mioFunc = MIO

class HasValue b i where
  val :: V b r a -> MIO i r a
instance HasValue Var i where
  val (R r) = MIO $ liftIO $ readIORef r
instance HasValue Val i where
  val (Lit v) = return v
instance HasValue b InLoop => HasValue (Comp InLoop b) InLoop where
  val (C m) = val =<< m
instance HasValue b InLoop => HasValue (Comp InFunction b) InLoop where
  val (C m) = val =<< toLoop m
instance HasValue b InFunction => HasValue (Comp InFunction b) InFunction where
  val (C m) = val =<< m


class State i where 
  getState :: MIO i r (Control i r)
  
  type RetTy i r 
  return' :: r -> MIO i r (RetTy i r)
  
  toLoop :: MIO i r a -> MIO InLoop r a  
  
instance State InFunction where 
  getState = mioFunc ask
  
  type RetTy InFunction a = a
  return' r = mioFunc $ do
    InFunction ret <- ask
    lift $ ret r
    return r

  toLoop (MIO m) = MIO $
    withReaderT (\(InLoop _ _ retLoop) -> InFunction retLoop) m

  
instance State InLoop     where 
  getState = mioLoop ask

  type RetTy InLoop a = ()
  return' r = mioLoop $ do
    InLoop _ _ ret <- ask
    lift $ ret r
    return ()
    
  toLoop m = m  
  
for' :: (State i, HasValue b i, HasValue b InLoop) => (MIO i r irr1, V b r Bool, MIO i r irr2) -> MIO InLoop r () -> MIO i r ()
for' (init, check, incr) (body :: MIO InLoop r ()) = do init 
                                                        for_r
    where for_r = do
            do_comp <- val check
            when do_comp $ callCC $ \break_foo -> do
                           callCC $ \continue_foo ->
                             MIO $ flip withReaderT (getMIO body) $ \inbod -> 
                               InLoop (toLoop $ break_foo ()) (toLoop $ continue_foo ()) (getReturn inbod)
                           incr
                           for_r
    

break' :: MIO InLoop r ()
break' = do
  InLoop b _ _ <- getState
  b

continue' :: MIO InLoop r ()
continue' = do
  InLoop _ c _ <- getState
  c


runImperative :: MIO InFunction a a -> IO a
runImperative foo = do
  a <- runContT (callCC $ \ret -> runReaderT (getMIO foo) $ InFunction ret) return
  return a

-- | @'function' foo@ takes an ImperativeMonad action and removes it from it's  
-- specific function context, specifically making it applicable 
-- in the body of other functions.
function :: MIO InFunction a a -> MIO i b a
function = MIO . liftIO . runImperative





instance Eq a => Eq (V Val r a) where
  (Lit a) == (Lit a') = a == a'
instance Show a => Show (V Val r a) where
  show (Lit a) = show a
instance Num a => Num (V Val r a) where
  (Lit a) + (Lit b) = Lit $ a + b
  (Lit a) * (Lit b) = Lit $ a * b
  abs (Lit a) = Lit $ abs a
  signum (Lit a) = Lit $ signum a
  fromInteger = Lit . fromInteger
instance IsString s => IsString (V Val r s) where
  fromString = Lit . fromString


-- | @('&')a@ gets a reference/pointer to the variable specified
(&) :: V Var r a -> V Var s a
(&) (R a) = R a


-- | 'auto' should just be used where the 
-- type can be automatically infered and we don't need an initial value
-- Use caution, as it is simply an alternate name for 'undefined'
auto = undefined

-- | 'new' constructs a new reference to the specified pure value
new :: a -> MIO i r (V Var r a)
new a = do
  r <- MIO $ liftIO $ newIORef a
  return $ R r


infixr 0 =:

-- | The 'Assignable' class is used to specify a value which can be 
-- computed imperatively.

class Assignable i valt where 
  -- | @variable '=:' value@ executes @value@ and writes it  
  -- to the location pointed to by @variable@
  (=:) :: V Var r a -> valt r a -> MIO i r () 

instance (HasValue b i) => Assignable i (V b) where  
  (=:) (R ar) br = MIO $ do
    b <- getMIO $ val br
    liftIO $ writeIORef ar b

instance Assignable i (MIO i) where  
  (=:) (R ar) br = do
    b <- br
    liftIO $ writeIORef ar b

-- | @'while''(check)@ acts like its imperative @while@ counterpart.
while' :: (HasValue b i, HasValue b InLoop, State i) => V b r Bool -> MIO InLoop r () -> MIO i r ()
while' check = for'(return (), check, return () )

-- | @'if''(check) act@ only performs @act@ if @check@ evaluates to true
-- it is specifically a value in its argument.
if' :: (HasValue b i) => V b r Bool -> MIO i r () -> MIO i r ()
if' b m = do
  v <- val b
  when v m

-- | @'modifyOp'@ makes a modification assignment operator 
-- out of a binary haskell function.
-- The suggested use is to replicate functionality of assignments
-- like @-=@ or @%=@ from C style languages.
modifyOp :: (HasValue k i) => (a->b->a) -> V Var r a -> V k r b -> MIO i r ()
modifyOp op (R ar) br = MIO $ do
  b <- getMIO $ val br
  liftIO $ modifyIORef ar (\v -> op v b)
