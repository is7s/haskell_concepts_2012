import Control.Monad.State.Strict
import Data.Map (Map,(!),insert,empty,notMember)
import Debug.Trace

data Term = C String | V Char | S String [Term] deriving (Show,Eq)

unify :: Term -> Term -> StateT (Map Char Term) Maybe (Map Char Term)
unify (V x)    (V y)     | x == y  = get
unify (V c)  x           = get >>= \env -> when (notMember c env) (modify (insert c x)) >> get >>= unify x . (!c)
unify p        v@(V _)   = unify v p
unify (C x)    (C y)     | x == y  = get
unify (S n t1) (S n' t2) | n == n' = zipWithM' unify t1 t2 >> get
unify _        _         = lift Nothing

zipWithM' :: (MonadPlus m, Functor m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM' _ []     []     = return []
zipWithM' f (x:xs) (y:ys) = (:) `fmap` f x y `ap` zipWithM' f xs ys
zipWithM' _ _      _      = mzero

(=?) :: Term -> Term -> Maybe (Map Char Term)
t1 =? t2 = evalStateT (unify t1 t2) empty
