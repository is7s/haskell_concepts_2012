import Control.Monad.State
import Data.Map (Map,(!),member,insert,empty)

data Term = C String | V Char | S String [Term] deriving (Show,Eq)

type Subs = [(Char,Term)]

unify :: Term -> Term -> StateT (Map Char Term) Maybe Subs
unify (V c) x = do
    env <- get
    let term = env!c
    if member c env then unify term x >>= lift . Just . ((c,term):)
        else modify (insert c x) >> lift (Just [(c,x)])
unify p@_ v@(V _) = unify v p
unify (C x) (C y) | x == y = lift $ Just []
unify (S n xs) (S n' ys) | n == n'   = zipWithM' unify xs ys >>= lift . Just . concat
unify _     _     = lift Nothing

zipWithM' :: MonadPlus m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' _ []     []     = return []
zipWithM' f (x:xs) (y:ys) = do
    a  <- f x y
    as <- zipWithM' f xs ys
    return (a:as)
zipWithM' _ _      _      = mzero

prolog :: Term -> Term -> Maybe Subs
prolog t1 t2 = runStateT (unify t1 t2) empty >>= Just . fst
