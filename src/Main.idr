import Syntax
import Parser
-- import Interpreter

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

parseGame' : (HasIO io) => {c : Bool} -> Grammar () Bits8 c a -> ReaderT UInt8Array (StateT Bits32 io) (Either ? a)
parseGame' = parseM

parseGame : (HasIO io) => UInt8Array -> io (List (LineNum, List1 Stmt))
parseGame buf = do
  r <- evalStateT (the Bits32 0) $ runReaderT buf $ parseGame' $ replicateM 1148 line
  pure $ case (the (Either (List1 (ParsingError Bits8)) ?) r) of
    Left errs => assert_total $ idris_crash "parse"
    Right x => x

-- partial main : IO ()
-- main = do
--   lines <- loadGame
--   let lines = sortBy (comparing fst) lines
--   -- traverse_ printLn lines

--   let (r, s) = startBASIC lines
--   let loop : S -> BASIC () -> IO ()
--       loop s act = do
--         let (s', out) = runBASIC r s act
--         printLn out
--         next <- case out of
--           WaitInput actions => do
--             let waitInput : IO (BASIC ())
--                 waitInput = do
--                   putStr "> "
--                   s <- toLower <$> getLine
--                   case words s of
--                     ["do", n] => pure $ playerAction $ cast n
--                     ["go", n] => pure $ playerMove $ cast n
--                     _ => waitInput
--             waitInput
--           _ => pure execLine
--         loop s' next
--   loop s execLine

record UI where
  constructor MkUI
  img : Image
  text : Pre
  prompt : Paragraph
  actions: UList

initUI : JSIO UI
initUI = do
  img <- newElement Ime [src =. "01.png"]

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

partial main : IO ()
main = runJS $ do
  ui <- initUI

  p <- fetch "http://localhost/po/patched.mem"
  p <- p `then_` arrayBuffer
  _ <- p `then_` \buf => do
    buf8 <- pure $ the UInt8Array $ cast buf
    pure $ traceConsole "Loaded" ()
    game <- parseGame buf8
    pure $ traceConsole "Parsed" ()
    textContent ui.text .= "Game loaded"
    pure $ ready ()
    -- pure $ ready $ traceConsole game ()

  pure ()
