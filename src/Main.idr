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

record Buf8Parser m a where
  constructor MkBuf8Parser
  runBufParser : ReaderT UInt8Array (StateT Bits32 m) a

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

parseGame : (HasIO io) => UInt8Array -> io (List (LineNum, List1 Stmt))
-- parseGame buf = do
--   r <- runReaderT buf $ evalStateT 0 $ parseM $ replicateM 1148 line
--   pure $ case r of
--     Left errs => assert_total $ idris_crash "parse"
--     Right x => x
-- parseGame bs = case parse (replicateM 1148 line) . map irrelevantBounds $ bs' of
--   Left errs => assert_total $ idris_crash "parse"
--   Right (x, rest) => x
--   where
--     patch : List Bits8 -> List Bits8
--     patch bs = let (pre, post) = splitAt (0x0803 + 28282) bs
--           in pre <+> [0x99] <+> post

--     bs' : List Bits8
--     bs' = drop 0x0803 . patch . drop 2 $ bs

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

-- record UI where
--   constructor MkUI
--   img : Image
--   text : Pre
--   prompt : Paragraph
--   actions: UList

-- initUI : JSIO UI
-- initUI = do
--   img <- newElement Ime [src =. "01.png"]

--   _ <- appendChild !body img

--   compass <- newElement Div [id =. "compass"]
--   for_ [(the String "n", "É"), ("s", "D"), ("e", "K"), ("w", "NY")] $ \(tag, lab) => do
--     span <- newElement Span [id =. ("compass-" <+> tag)]
--     a <- newElement A  [href =. "", textContent =. lab]
--     _ <- appendChild span a
--     _ <- appendChild compass span
--     pure ()
--   _ <- appendChild !body compass

--   text <- createElement Pre
--   _ <- appendChild !body text

--   prompt <- newElement P [id =. "prompt"]
--   _ <- appendChild !body prompt

--   actions <- createElement Ul
--   _ <- appendChild !body actions

--   pure $ MkUI
--     { img = img
--     , text = text
--     , prompt = prompt
--     , actions = actions
--     }

-- later : Promise a -> Union2 a (Promise a)
-- later x = toUnion2 $ inject x

later : Promise a -> NS I [a, Promise a]
later x = inject x

now : a -> NS I [a, Promise a]
now x = inject x

-- arrayToListIO : (HasIO io, ArrayLike arr a) => arr -> io (List a)
-- arrayToListIO {io = io} {a = a} v = go 0 []
--   where
--     go : Bits32 -> List a -> io (List a)
--     go i acc = do
--       mx <- readIO v i
--       case mx of
--         Nothing => pure $ reverse acc
--         Just x => go (i + 1) (x :: acc)

partial main : IO ()
main = runJS $ do
  -- ui <- initUI

  p <- fetch "http://localhost/po/pokol.mem"
  p <- p `then_` arrayBuffer
  _ <- p `then_` \buf => do
    buf8 <- pure $ the UInt8Array $ cast buf
    game <- parseGame buf8
    pure $ ready $ traceConsole game ()

  pure ()
