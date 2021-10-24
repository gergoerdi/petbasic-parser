module JS.App

import Data.IORef

public export
record App (m : Type -> Type) (input : Type) (output : Type) where
  constructor MkApp
  view : (input -> m ()) -> m (output -> m ())
  model : input -> m output
  initial : output

export runApp : HasIO m => App m input output -> m ()
runApp app = do
  box <- newIORef (\i => pure ())
  refresh <- app.view (\i => readIORef box >>= ($ i))
  writeIORef box $ \i => app.model i >>= refresh
  refresh app.initial
