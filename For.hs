-----------------------------------------------------------------------------
-- |
-- Module      :  For
-- Author      :  Matthew Mirman ( mmirman@andrew.cmu.edu )
-- Stability   :  experimental
-- Portability :  portable
--
-- A module for C Style haskell code.
--
-----------------------------------------------------------------------------

{-# LANGUAGE                                                                                                                                         
 GADTs,                                                                                                                                                        
 EmptyDataDecls                                                                                                                                         
 #-}
module For where
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

liftF :: MIO a a -> IO a
liftF foo = runContT (callCC $ \ret -> runReaderT foo $ InFunction ret) return

function :: MIO a a -> MIO b a
function = liftIO . liftF

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
val val = case val of
  R r -> liftIO $ readIORef r
  L v -> return v
  C m -> val' =<< m
val' = val

ref :: V Var r a -> V Var s a
ref (R a) = R a

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

modifyOp :: (a->b->a) -> V Var r a -> V k r b -> MIO r ()
modifyOp op (R ar) br = do
  b <- val br
  liftIO $ modifyIORef ar (\v -> op v b)

(+=:) = modifyOp (+)
(*=:) = modifyOp (*)
(-=:) = modifyOp (-)
(%=:) = modifyOp (mod)


liftOp2 foo ar br = C $ do
  a <- val ar
  b <- val br
  return $ prim $ foo a b

(<.) = liftOp2 (<)
(>.) = liftOp2 (>)
(+.) = liftOp2 (+)
(*.) = liftOp2 (*)

for:: (MIO r irr1, V b r Bool, MIO r irr2) -> MIO r () -> MIO r ()
for (init, check, incr) body = init >> for'
  where for' = do
          do_comp <- val check
          when do_comp $ callCC $ \break_foo -> do
                         callCC $ \continue_foo -> do
                           flip withReaderT body $ \inbod ->
                             InLoop (break_foo ()) (continue_foo ()) (controlReturn inbod)
                         incr
                         for'

iff:: V b r Bool -> MIO r () -> MIO r ()
iff b m = do
  v <- val b
  when v m


swap(r1, r2) = function $ do
  {
    z <- auto undefined;
    z =: r1;
    r1 =: r2;
    r2 =: z;
  };

factorial = function $ do
  {
    a <- auto 0;
    n <- auto 1;
    for ( a =: prim 1 , a <. prim 11 , a +=: prim 1 ) $ do
      {
        n *=: a;
        iff ( a <. prim 7) $ do
          {
            continue;
          };

        iff ( a >. prim 5) $ do
          {
            break;
          };
      };

    swap( ref n , ref a);

    returnF n;
  };

 
main = do
  t <- liftF factorial
  putStrLn $ "MAGIC FACTORIAL: "++show t
