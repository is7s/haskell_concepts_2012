import Control.Monad.State.Strict
import Data.Map (Map,(!),insert,empty,notMember)
import Debug.Trace

data Term = C String | V Char | S String [Term] deriving (Show,Eq)

unify :: Term -> Term -> StateT (Map Char Term) Maybe (Map Char Term)
unify (V x)    (V y)     | x == y  = get
unify (V c)  x           = get >>= \env -> when (notMember c env) (modify (insert c x)) >> get >>= unify x . (!c)
unify p        v@(V _)   = unify v p
unify (C x)    (C y)     | x == y  = get
unify (S n t1) (S n' t2) | n == n' = zipWithM'_ unify t1 t2 >> get
unify _        _         = lift Nothing

zipWithM'_ :: (MonadPlus m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM'_ _ []     []     = return ()
zipWithM'_ f (x:xs) (y:ys) = f x y >> zipWithM'_ f xs ys
zipWithM'_ _ _      _      = mzero

(=?) :: Term -> Term -> Maybe (Map Char Term)
t1 =? t2 = evalStateT (unify t1 t2) empty
