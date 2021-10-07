module Control.Monad.Cont

-- import public Control.Monad.Identity -- left here for compatibility
-- import public Control.Monad.Trans -- left here for compatibility
import public Control.Monad.Cont.Interface as Control.Monad.Cont
import public Control.Monad.Cont.Cont as Control.Monad.Cont

-- TODO: these should be moved to the relevant modules
import Control.Monad.State
import Control.Monad.Reader

public export %inline
implementation MonadCont m => MonadCont (ReaderT r m) where
  callCC f =
    MkReaderT $ \r =>
    callCC $ \k =>
    runReaderT r $
    f $ \x => MkReaderT $ \_ => k x

public export %inline
implementation MonadCont m => MonadCont (StateT s m) where
  callCC f =
    ST $ \s =>
    callCC $ \k =>
    runStateT s $
    f $ \x => ST $ \s' => k (s', x)
