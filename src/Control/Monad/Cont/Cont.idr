module Control.Monad.Cont.Cont

import Control.Monad.Trans
-- import Control.Monad.Cont.Interface

public export
record ContT (r : Type) (m : Type -> Type) (a : Type) where
  constructor CT
  runContT : (a -> m r) -> m r

public export
implementation Functor (ContT r f) where
  map f m = CT $ \k => runContT m (k . f)

public export
implementation Applicative (ContT r f) where
  pure x = CT ($ x)
  f <*> v = CT $ \k => runContT f $ \g => runContT v (k . g)

public export
implementation Monad (ContT r m) where
  m >>= n  = CT $ \k => runContT m $ \x => runContT (n x) k

public export
implementation MonadTrans (ContT r) where
  lift m = CT (m >>=)
