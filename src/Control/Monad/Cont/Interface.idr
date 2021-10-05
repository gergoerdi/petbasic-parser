module Control.Monad.Cont.Interface

import Control.Monad.Cont.Cont

public export
interface Monad m => MonadCont m where
  callCC : ((a -> m b) -> m a) -> m a

public export %inline
implementation MonadCont (ContT r m) where
  callCC f = CT $ \ k => runContT (f (\ x => CT $ \_ => k x)) k
