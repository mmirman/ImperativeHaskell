{-# LANGUAGE
 TemplateHaskell
 #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Imperative.FunctionFactory
-- Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
-- Stability   :  experimental
-- Portability :  TemplateHaskell
-- A module which defines a function 'liftOp' which coverts pure functions 
-- into reference taking functions.
-----------------------------------------------------------------------------

module Control.Monad.Imperative.FunctionFactory
       ( liftOp
       ) where

import Control.Monad.Imperative.Internals
import Language.Haskell.TH
import Data.Functor

-- | @'liftOp' nm@ is a function factory producer which uses template 
-- to infer the type of its argument and output an impure function factory.
-- 
-- The argument @nm@ must be a quoted name with type already known.
-- 
-- Examples:
-- > liftSomeFoo (v :: a -> ((a,b),b) -> c) = $(liftOp 'v)
-- >
-- > plus = $(liftOp '(+))
-- > 
-- > letBound = let f a = a * 5 in $(liftOp 'f)
-- > 
-- > argument n = $(liftOp 'mod) 4 n
-- 
-- If the function factories produce will be used in the same file that produced them
-- the output type needs to be known before 'liftOp' is called, 
-- such as in the definition of 'liftOp2'.
-- 
-- Currently 'liftOp' does not support unboxed tuples.
liftOp :: Name -> Q Exp
liftOp nm = do
  let getTy (VarI _ ty _ _) = ty
      getTy (ClassOpI _ ty _ _) = ty
      getTy (DataConI _ ty _ _) = ty
      getTy r = error $ "liftOp only supports lifting variable names. Tried to show" ++show r
  ty <- getTy <$> reify nm
  (pats,pats') <- unzip <$> (sequence $ tyToPats ty)
  lamE (map return pats) $ appE [| C |] $ doE $
    (nn2st <$> zip (ps2ns pats) (ps2ns pats'))
    ++
    [noBindS $ appE [| return . Lit |] $ return $ foldl AppE (VarE nm) $ map p2e pats']

tyToPats :: Type -> [Q (Pat, Pat)]
tyToPats (ForallT _ _ t) = tyToPats t
tyToPats (AppT (AppT ArrowT a) r) = typat:tyToPats r
  where typat = do
          t <- t2p a
          return $ case t of
            (TupP s, TupP s') -> (TupP $ reverse s, TupP $ reverse s')
            _ -> t

        isTuple (AppT v _) = isTuple v
        isTuple (TupleT _) = True
        isTuple _ = False    
        
        t2p (ForallT _ _ t) = t2p t
        t2p (TupleT _) = return (TupP [], TupP [])
        t2p (AppT v a) | isTuple v = do
          (TupP t, TupP t') <- t2p v
          (p,p') <- t2p a
          return (TupP $ p:t, TupP $ p':t')
        t2p _ = do
          a <- newName "a"
          b <- newName "b"
          return $ (VarP a, VarP b)
tyToPats _ = []

p2e :: Pat -> Exp
p2e (TupP l) = TupE $ map p2e l
p2e (VarP n) = VarE n
p2e l = error $ "Bug: shouldn't encounter non tuples or variables here. p2e: " ++ show l

ps2ns :: [Pat] -> [Name]
ps2ns = concatMap p2ns
  where p2ns (TupP l) = ps2ns l
        p2ns (VarP n) = [n]
        p2ns l = error $ "Bug: shouldn't encounter non tuples or variables here. ps2ns: " ++ show l

nn2st :: (Name,Name) -> StmtQ
nn2st (n,n') = bindS (varP n') [| val $(varE n) |]