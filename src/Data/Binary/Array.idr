module Data.Binary.Array

import Data.Binary

import Control.Monad.Reader
import Control.Monad.State

import JS

public export
Get : Type -> Type
Get = ReaderT UInt8Array (StateT Bits32 IO)

public export
implementation MonadGet Get where
  getBits8 = do
    i <- lift $ get <* modify (+ 1)
    arr <- ask
    mx <- readIO arr i
    pure $ assert_total $ case mx of Just x => x

public export
Put : Type -> Type
Put = ReaderT (Array Bits8) (StateT Bits32 IO)

public export
implementation MonadPut Put where
  putBits8 x = do
    i <- lift $ get <* modify (+ 1)
    arr <- ask
    writeIO arr i x
