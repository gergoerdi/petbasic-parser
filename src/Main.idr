import PETBASIC.Syntax
import PETBASIC.Binary
import Pokol.UI

import Data.Binary
import JS.App

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

%foreign "browser:lambda:(s) => Uint8Array.from(s, c=>c.charCodeAt(0))"
prim__stringToUInt8Array : String -> UInt8Array

%foreign "browser:lambda:() => atob(ppb64)"
prim__ppbImage : () -> String

main : IO ()
main = runJS $ do
  let buf8 = prim__stringToUInt8Array $ prim__ppbImage ()
  lines <- loadGame buf8
  app lines >>= runApp
  pure ()
