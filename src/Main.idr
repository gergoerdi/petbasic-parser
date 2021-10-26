import PETBASIC.Syntax
import PETBASIC.Binary
import Pokol.UI

import Data.Binary
import JS.App
import JS.Promise.Extra

import Data.List1
import Control.Monad.Reader
import Control.Monad.State

import JS
import Web.Fetch
import Web.Dom
import Web.Html
import Web.Raw.UIEvents
import Web.Raw.Fetch

loadGame : HasIO io => UInt8Array -> io (List (LineNum, List1 Stmt))
loadGame buf = liftIO $ evalStateT (the Bits32 0) $ runReaderT buf $ getList getLine
  where
    getLine : Get (LineNum, List1 Stmt)
    getLine = (,) <$> get <*> getList1 get

main : IO ()
main = runJS $ do
  p <- fetch "assets/pokol.ppb"
  p <- p `then_` arrayBuffer
  _ <- p `then_` \buf => do
    buf8 <- pure $ the UInt8Array $ cast buf
    putStrLn "Loaded"
    lines <- loadGame buf8
    putStrLn "Parsed"
    app lines >>= runApp
    pure $ ready ()

  pure ()
