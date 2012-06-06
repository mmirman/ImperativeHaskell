{-# LANGUAGE
 GADTs,
 GeneralizedNewtypeDeriving,
 MultiParamTypeClasses,
 FlexibleInstances,
 FlexibleContexts,
 UndecidableInstances,
 TypeFamilies,
 FunctionalDependencies,
 DataKinds
 #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.Internals
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  GADTs, EmptyDataDecls, GeneralizedNewtypeDeriving, 
--                MultiParamTypeClasses, FlexibleInstances
-- 
-- A module which defines the monad for ImperativeHaskell,  
-- and some control operator to interact with 'MIO'
-----------------------------------------------------------------------------
module Control.Monad.Imperative.Internals
       ( modifyOp
       , if'
       , for'
       , while'
       , break'
       , continue'
       , function 
       , new
       , auto
       , runImperative
       , V(Lit, C)       
       , MIO()
       , ValueKind(..)
       , ControlKind(..)         
       , (=:)
       , (&)
       , HasValue(..)
       , State(return')
       )  where

import Control.Monad.Cont
import Control.Monad.Reader
import Data.IORef
import Data.String (IsString(..))

data ControlKind = TyInLoop
                 | TyInFunc

data ValueKind = TyVar
               | TyVal
               | TyComp ControlKind ValueKind
                 
type RCont r = ContT r IO
type MIO_I i r a = ReaderT (Control i r) (RCont r) a
type RetCont r = r -> RCont r ()

newtype MIO i r a = MIO { getMIO :: MIO_I i r a }
                  deriving (Monad, MonadCont, MonadIO)
  
data V (b :: ValueKind) r a where
  R :: IORef a -> V TyVar r a
  Lit :: a -> V TyVal r a
  C :: MIO i r (V b r a) -> V (TyComp i b) r a
  
data Control (i :: ControlKind) r where  
  InFunction :: RetCont r -> Control TyInFunc r
  InLoop :: MIO TyInLoop r () -> MIO TyInLoop r () -> RetCont r -> Control i r

getReturn :: Control i r -> RetCont r
getReturn (InFunction ret) = ret
getReturn (InLoop _ _ ret) = ret

-- | Although the functional dependency @b -> i@ is declared, 
-- it does not do anything useful. 
class HasValue b (i :: ControlKind) | b -> i where
  val :: b r a -> MIO i r a
instance HasValue (V TyVar) i where
  val (R r) = MIO $ liftIO $ readIORef r
instance HasValue (V TyVal) i where
  val (Lit v) = return v
instance HasValue (V b) a => HasValue (V (TyComp a b)) a where
  val (C m) = val =<< m
instance HasValue (MIO i) i where
  val m = m

class State (i :: ControlKind) where 
  type RetTy i a
  getState :: MIO i r (Control i r)
  
  -- | @'return'' value@ acts like an imperative return. It passes
  -- the given value to the return continuation.
  return' :: HasValue (V a) i => V a r r -> MIO i r (RetTy i r)
  toLoop :: MIO i r a -> MIO TyInLoop r a  
  
instance State TyInFunc where 
  type RetTy TyInFunc a = a
  getState = MIO ask
  return' v = MIO $ do
    v' <- getMIO $ val v
    InFunction ret <- ask
    lift $ ret v'
    return v'

  toLoop (MIO m) = MIO $
    withReaderT (\(InLoop _ _ retLoop) -> InFunction retLoop) m
  
instance State TyInLoop where 
  type RetTy TyInLoop a = ()
  getState = MIO ask
  return' v = MIO $ do
    v' <- getMIO $ val v
    InLoop _ _ ret <- ask
    lift $ ret v'
    return ()

  toLoop m = m  
  
-- | @'for''(init, check, incr)@ acts like its imperative @for@ counterpart
for' :: (State i, HasValue (V b) i) => (MIO i r irr1, V b r Bool, MIO i r irr2) -> MIO TyInLoop r () -> MIO i r ()
for' (init, check, incr) body = init >> for_r
    where for_r = do
            do_comp <- val check
            when do_comp $ callCC $ \break_foo -> do
                           callCC $ \continue_foo -> MIO $ do
                             flip withReaderT (getMIO body) $ \inbod -> 
                               InLoop (toLoop $ break_foo ()) (toLoop $ continue_foo ()) (getReturn inbod)
                           incr
                           for_r

-- | @'break''@ exists the current loop.  
break' :: MIO TyInLoop r ()
break' = do
  InLoop b _ _ <- getState
  b

-- | 'continue'' continues the current loop, passing over
-- any control flow that is defined.
continue' :: MIO TyInLoop r ()
continue' = do
  InLoop _ c _ <- getState
  c

-- | @'runImperative'@ takes an MIO action as returned by a function, 
-- and lifts it into IO.
runImperative :: MIO TyInFunc a a -> IO a
runImperative foo = 
  runContT (callCC $ \ret -> runReaderT (getMIO foo) (InFunction ret)) return

-- | @'function' foo@ takes an ImperativeMonad action and removes it from it's  
-- specific function context, specifically making it applicable 
-- in the body of other functions.
function ::  MIO TyInFunc a a -> MIO i b a
function = MIO . liftIO . runImperative

instance Eq a => Eq (V TyVal r a) where
  (Lit a) == (Lit a') = a == a'
instance Show a => Show (V TyVal r a) where
  show (Lit a) = show a
instance Num a => Num (V TyVal r a) where
  (Lit a) + (Lit b) = Lit $ a + b
  (Lit a) * (Lit b) = Lit $ a * b
  abs (Lit a) = Lit $ abs a
  signum (Lit a) = Lit $ signum a
  fromInteger = Lit . fromInteger
instance IsString s => IsString (V TyVal r s) where
  fromString = Lit . fromString


-- | @('&')a@ gets a reference/pointer to the variable specified
(&) :: V TyVar r a -> V TyVar s a
(&) (R a) = R a


-- | 'auto' should just be used where the 
-- type can be automatically infered and we don't need an initial value
-- Use caution, as it is simply an alternate name for 'undefined'
auto = undefined

-- | 'new' constructs a new reference to the specified pure value
new :: (HasValue (V TyVar) i) => a -> MIO i r (V TyVar r a)
new a = do
  r <- MIO $ liftIO $ newIORef a
  return $ R r

infixr 0 =:

-- | @variable '=:' value@ executes @value@ and writes it  
-- to the location pointed to by @variable@
(=:) :: (HasValue valt i, HasValue (V TyVar) i) => V TyVar r a -> valt r a -> MIO i r () 
(R ar) =: br = MIO $ do
  b <- getMIO $ val br
  liftIO $ writeIORef ar b

-- | @'while''(check)@ acts like its imperative @while@ counterpart.
while' :: (HasValue (V b) i, HasValue (V b) TyInLoop, State i) => V b r Bool -> MIO TyInLoop r () -> MIO i r ()
while' check = for'(return (), check, return () )

-- | @'if''(check) act@ only performs @act@ if @check@ evaluates to true
-- it is specifically a value in its argument.
if' :: (HasValue (V b) i) => V b r Bool -> MIO i r () -> MIO i r ()
if' b m = do
  v <- val b
  when v m

-- | @'modifyOp'@ makes a modification assignment operator 
-- out of a binary haskell function.
-- The suggested use is to replicate functionality of assignments
-- like @-=@ or @%=@ from C style languages.
modifyOp :: (HasValue (V TyVar) i, HasValue (V k) i) => (a->b->a) -> V TyVar r a -> V k r b -> MIO i r ()
modifyOp op (R ar) br = MIO $ do
  b <- getMIO $ val br
  liftIO $ modifyIORef ar (\v -> op v b)
