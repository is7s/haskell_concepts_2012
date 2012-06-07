import Control.Monad.State
import Data.Map (Map,(!),insert,empty,member)

data Term = C String | V Char | S String [Term] deriving (Show,Eq)

unify :: Term -> Term -> StateT (Map Char Term) Maybe ()
unify (V x)    (V y)     | x == y  = return ()
unify (V c)  x           = get >>= \env -> if member c env then unify x (env!c) else modify (insert c x)
unify p        v@(V _)   = unify v p
unify (C x)    (C y)     | x == y  = return ()
unify (S n t1) (S n' t2) | n == n' = zipWithM'_ unify t1 t2
unify _        _         = lift Nothing

zipWithM'_ :: (MonadPlus m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM'_ _ []     []     = return ()
zipWithM'_ f (x:xs) (y:ys) = f x y >> zipWithM'_ f xs ys
zipWithM'_ _ _      _      = mzero

(=?) :: Term -> Term -> Maybe (Map Char Term)
t1 =? t2 = execStateT (unify t1 t2) empty
