module Data.Binary

public export
interface Monad m => MonadGet m where
  getBits8 : m Bits8

public export
interface Monad m => MonadPut m where
  putBits8 : Bits8 -> m ()

public export
interface Binary a where
  get : MonadGet m => m a
  put : MonadPut m => a -> m ()

public export
implementation Binary Bits8 where
  get = getBits8
  put = putBits8
