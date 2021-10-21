import Syntax
import Binary
import Interpreter

import Text.Parser as P
import Data.List
-- import Data.Maybe
-- import Data.String

import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.Maybe
-- import Control.Monad.Either

import JS
import Web.Fetch
import Web.Dom
import Web.Html
import Web.Raw.UIEvents
import Web.Raw.Fetch

%hide Array.fromString
%hide Text.Parser.Core.(>>=)

%foreign "browser:lambda:(_a, x) => ((console.log(x),x))"
traceConsoleId : a -> a

%foreign "browser:lambda:(_a, _b, x, y) => ((console.log(x),y))"
traceConsole : a -> b -> b

replicateM : (n : Nat) -> Grammar st k True a -> Grammar st k (n > 0) (List a)
replicateM 0 _ = pure []
replicateM (S n) act = (::) <$> act <*> replicateM n act

(HasIO io) => MonadToken (ReaderT UInt8Array (StateT Bits32 io)) Bits8 where
  nextToken = do
    v <- ask
    i <- get
    mx <- readIO v i
    case mx of
      Nothing => pure Nothing
      Just x => do
        lift $ modify (+ 1)
        pure $ Just $ irrelevantBounds x -- TODO: can we come up with a bound?

  push act = do
    i <- get
    x <- act
    pure (x, put i)

loadGame : (HasIO io) => UInt8Array -> io (List (LineNum, List1 Stmt))
loadGame buf = liftIO $ evalStateT (the Bits32 0) $ runReaderT buf $ loadList loadLine
  where
    loadLine : Get (LineNum, List1 Stmt)
    loadLine = (,) <$> load <*> loadList1 load

record UI where
  constructor MkUI
  img : Image
  text : Pre
  prompt : Paragraph
  actions: UList

initUI : JSIO UI
initUI = do
  img <- createElement Ime

  _ <- appendChild !body img

  compass <- newElement Div [id =. "compass"]
  for_ [(the String "n", "É"), ("s", "D"), ("e", "K"), ("w", "NY")] $ \(tag, lab) => do
    span <- newElement Span [id =. ("compass-" <+> tag)]
    a <- newElement A  [href =. "", textContent =. lab]
    _ <- appendChild span a
    _ <- appendChild compass span
    pure ()
  _ <- appendChild !body compass

  text <- createElement Pre
  _ <- appendChild !body text

  prompt <- newElement P [id =. "prompt"]
  _ <- appendChild !body prompt

  actions <- createElement Ul
  _ <- appendChild !body actions

  pure $ MkUI
    { img = img
    , text = text
    , prompt = prompt
    , actions = actions
    }

elementList : HTMLCollection -> JSIO (List Element)
elementList coll = do
  n <- HTMLCollection.length coll
  let loop : Bits32 -> JSIO (List Element)
      loop i = do
        if i < n
          then do
            mx <- HTMLCollection.item coll i
            case mx of
              Nothing => pure []
              Just x => (x ::) <$> loop (i + 1)
          else pure []
  loop 0

partial step : UI -> R -> S -> JSIO S
step ui r s = do
  -- let loop : S -> BASIC () -> JSIO ()
  --     loop s act = do
  --       let (s', out) = runBASIC r s act
  --       printLn out
  --       next <- case out of
  --         WaitInput actions => do
  --           let waitInput : IO (BASIC ())
  --               waitInput = do
  --                 putStr "> "
  --                 s <- toLower <$> getLine
  --                 case words s of
  --                   ["do", n] => pure $ playerAction $ cast n
  --                   ["go", n] => pure $ playerMove $ cast n
  --                   _ => waitInput
  --           waitInput
  --         _ => pure execLine
  --       loop s' next
  -- loop s execLine

  let loop : S -> JSIO S
      loop s = do
        let (s', out) = runBASIC r s execLine
        case out of
          ChangeRoom pic txt => do
            src ui.img .= pic <+> ".png"
            textContent ui.text .= txt
            loop s'
          WaitInput actions => do
            oldActions <- elementList =<< children ui.actions
            traverse_ (removeChild ui.actions) oldActions
            for_ actions $ \action => do
              a <- newElement A [textContent =. action, href =. ""]
              li <- createElement Li
              ignore $ appendChild li a
              ignore $ appendChild ui.actions li
            pure s'
          EndGame => pure s'
          _ => do
            printLn out
            pure s'
  loop s

partial main : IO ()
main = runJS $ do
  ui <- initUI

  p <- fetch "http://localhost/po/pokol.ppb"
  p <- p `then_` arrayBuffer
  _ <- p `then_` \buf => do
    buf8 <- pure $ the UInt8Array $ cast buf
    printLn "Loaded"
    lines <- loadGame buf8
    printLn "Parsed"
    let (r, s) = startBASIC lines
    s' <- step ui r s
    pure $ ready ()

  pure ()
