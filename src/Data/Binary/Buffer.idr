module Data.Binary.Buffer

import Data.Binary
import Data.Buffer
import Control.Monad.Reader
import Control.Monad.State

public export
Put : Type -> Type
Put = ReaderT Buffer (StateT Int IO)

public export
implementation MonadPut Put where
  putBits8 x = do
    buf <- ask
    i <- lift $ get <* modify (+ 1)
    setBits8 buf i x
