module Data.Binary.File

import Data.Binary
import System.File
import Control.Monad.Reader

public export
Put : Type -> Type
Put = ReaderT File IO

public export
implementation MonadPut Put where
  putBits8 x = do
    h <- ask
    ignore $ fPutStr h $ pack [cast x]
