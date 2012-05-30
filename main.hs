import Control.Monad.State
import Data.Map (Map,(!),member,insert,empty)

data Term = C String | V Char | S String [Term] deriving (Show,Eq)

unify :: Term -> Term -> StateT (Map Char Term) Maybe [(Char,Term)]
unify (V c) x = get >>= \env -> let term = env!c in
    if member c env then unify term x >>= lift . Just . ((c,term):)
        else modify (insert c x) >> lift (Just [(c,x)])
unify p@_ v@(V _) = unify v p
unify (C x) (C y) | x == y = lift $ Just []
unify (S n xs) (S n' ys) | n == n'   = zipWithM' unify xs ys >>= lift . Just . concat where
    zipWithM' _ []     []     = return []
    zipWithM' f (x:xs) (y:ys) = (:) `fmap` f x y `ap` zipWithM' f xs ys
    zipWithM' _ _      _      = mzero
unify _     _     = lift Nothing

prolog :: Term -> Term -> Maybe [(Char,Term)]
prolog t1 t2 = runStateT (unify t1 t2) empty >>= Just . fst
