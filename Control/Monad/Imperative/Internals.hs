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
       , defer'
       , function 
       , new
       , auto
       , runImperative
       , io
       , V(Lit, C)       
       , MIO()
       , ValueKind(..)
       , ControlKind(..)         
       , (=:)
       , (&)
       , HasValue(..)
       , CState(return')
       ) where

import Data.Functor
import Control.Monad.Cont
import Control.Monad.State
import Data.IORef
import Data.String (IsString(..))

data ControlKind = TyInLoop
                 | TyInFunc

data ValueKind = TyVar
               | TyVal
               | TyComp ControlKind ValueKind
                 
type RCont r = ContT r IO
type MIO_I i r a = StateT (Control i r) (RCont r) a
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
class HasValue r b (i :: ControlKind) | b -> r i where
  val :: b a -> MIO i r a
instance HasValue r (V TyVar r) i where
  val (R r) = MIO $ liftIO $ readIORef r
instance HasValue r (V TyVal r) i where
  val (Lit v) = return v
instance HasValue r (V b r) a => HasValue r (V (TyComp a b) r) a where
  val (C m) = val =<< m
instance HasValue r (MIO i r) i where
  val m = m

instance HasValue r IO i where
  val m = liftIO m  

class CState (i :: ControlKind) where 
  type RetTy i a
  getState :: MIO i r (Control i r)
  
  -- | @'return'' value@ acts like an imperative return. It passes
  -- the given value to the return continuation.
  return' :: HasValue r (V a r) i => V a r r -> MIO i r (RetTy i r)
  toLoop :: MIO i r a -> MIO TyInLoop r a  
  
instance CState TyInFunc where 
  type RetTy TyInFunc a = a
  getState = MIO get
  return' v = MIO $ do
    v' <- getMIO $ val v
    InFunction ret <- get
    lift $ ret v'
    return v'

  toLoop (MIO m) = MIO $
    wrapState m statefulRetCont $ \(InLoop _ _ retLoop) -> InFunction retLoop

instance CState TyInLoop where 
  type RetTy TyInLoop a = ()
  getState = MIO get
  return' v = MIO $ do
    v' <- getMIO $ val v
    InLoop _ _ ret <- get
    lift $ ret v'
    return ()

  toLoop m = m  

statefulRetCont :: Control t r -> Control i r -> Control t r
statefulRetCont (InLoop a b _) = InLoop a b . getReturn
statefulRetCont (InFunction _) = InFunction . getReturn

-- | @'for''(init, check, incr)@ acts like its imperative @for@ counterpart
for' :: (CState i, HasValue r (V b r) i, HasValue r valt TyInLoop) => (MIO i r irr1, V b r Bool, MIO i r irr2) -> valt () -> MIO i r ()
for' (init, check, incr) body = init >> for_r
    where for_r = do
            do_comp <- val check
            when do_comp $ callCC $ \break_foo -> do
                           callCC $ \continue_foo -> MIO $
                             wrapState (getMIO $ val body) statefulRetCont $ \inbod -> 
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

runWithRet :: MIO TyInFunc r a-> RetCont r -> RCont r a
runWithRet m r = fmap fst $ runStateT (getMIO m) $ InFunction r

defer' :: HasValue r valt TyInFunc  => valt a -> MIO i r ()
defer' m = MIO $ do
  c <- get
  put $ case c of 
    InLoop a b r -> InLoop a b $ \i -> runWithRet (val m) r >> r i
    InFunction r -> InFunction $ \i -> runWithRet (val m) r >> r i
    
-- | @'runImperative'@ takes an MIO action as returned by a function, 
-- and lifts it into IO.
runImperative :: MIO TyInFunc a a -> IO a
runImperative foo = 
  runContT (callCC $ \ret -> fst <$> runStateT (getMIO foo) (InFunction ret)) return 


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
new :: (HasValue r (V TyVar r) i) => a -> MIO i r (V TyVar r a)
new a = do
  r <- MIO $ liftIO $ newIORef a
  return $ R r

infixr 0 =:

-- | @variable '=:' value@ executes @value@ and writes it  
-- to the location pointed to by @variable@
(=:) :: (HasValue r valt i, HasValue r (V TyVar r) i) => V TyVar r a -> valt a -> MIO i r () 
(R ar) =: br = MIO $ do
  b <- getMIO $ val br
  liftIO $ writeIORef ar b

-- | @'while''(check)@ acts like its imperative @while@ counterpart.
while' :: (HasValue r (V b r) i, HasValue r (V b r) TyInLoop, HasValue r valt TyInLoop, CState i) => V b r Bool -> valt () -> MIO i r ()
while' check = for'(return (), check, return () )

-- | @'if''(check) act@ only performs @act@ if @check@ evaluates to true
-- it is specifically a value in its argument.
if' :: (HasValue r (V b r) i, HasValue r valt i) => V b r Bool -> valt () -> MIO i r ()
if' b m = do
  v <- val b
  when v (val m)

-- | @'modifyOp'@ makes a modification assignment operator 
-- out of a binary haskell function.
-- The suggested use is to replicate functionality of assignments
-- like @-=@ or @%=@ from C style languages.
modifyOp :: (HasValue r (V TyVar r) i, HasValue r (V k r) i) => (a->b->a) -> V TyVar r a -> V k r b -> MIO i r ()
modifyOp op (R ar) br = MIO $ do
  b <- getMIO $ val br
  liftIO $ modifyIORef ar (\v -> op v b)

wrapState :: Monad m => StateT s m a -> (s' -> s -> s') -> (s' -> s) -> StateT s' m a
wrapState st fOut fIn = do
  sp <- get
  (a, s) <- lift $ runStateT st $ fIn sp
  put $ fOut sp s
  return a

-- | @'io' action@ takes a haskell 'IO' @action@ and makes it useable from within
-- the MIO monad.
io :: IO a -> MIO TyInFunc r a
io = liftIO