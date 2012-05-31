import Control.Monad.State
import Data.Map (Map,(!),member,insert,empty,toList)

data Term = C String | V Char | S String [Term] deriving (Show,Eq)

unify :: Term -> Term -> State (Map Char Term) Bool
unify (V c)    x                   = get >>= \env -> if member c env 
    then unify (env!c) x else modify (insert c x) >> return True
unify p        v@(V _)             = unify v p
unify (C x)    (C y)     | x == y  = return True
unify (S n t1) (S n' t2) | n == n' = go t1 t2 where
    go []     []     = return True
    go (x:xs) (y:ys) = (&&) `fmap` unify x y `ap` go xs ys
    go _      _      = return False
unify _        _                   = return False

(=?) :: Term -> Term -> Maybe [(Char,Term)]
t1 =? t2 = let (a,s) = runState (unify t1 t2) empty in if a then Just (toList s) else Nothing
