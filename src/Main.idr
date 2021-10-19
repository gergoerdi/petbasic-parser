-- import Syntax
-- import Parser
-- import Interpreter

-- import Text.Parser
-- import Data.List
-- import Data.Maybe
-- import Data.String
-- import System

-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Maybe
-- import Control.Monad.Either

import JS
import Web.Fetch
import Web.Dom
import Web.Html
import Web.Raw.UIEvents
import Web.Raw.Fetch

%hide Array.fromString

%foreign "browser:lambda:(_a, x) => ((console.log(x),x))"
traceConsoleId : a -> a

-- replicateM : (n : Nat) -> Grammar st k True a -> Grammar st k (n > 0) (List a)
-- replicateM 0 _ = pure []
-- replicateM (S n) act = (::) <$> act <*> replicateM n act

-- parseGame : List Bits8 -> List (LineNum, List1 Stmt)
-- parseGame bs = case parse (replicateM 1148 line) . map irrelevantBounds $ bs' of
--   Left errs => assert_total $ idris_crash "parse"
--   Right (x, rest) => x
--   where
--     patch : List Bits8 -> List Bits8
--     patch bs = let (pre, post) = splitAt (0x0803 + 28282) bs
--           in pre <+> [0x99] <+> post

--     bs' : List Bits8
--     bs' = drop 0x0803 . patch . drop 2 $ bs

-- loadGame : IO (List (LineNum, List1 Stmt))
-- loadGame = do
--     let fn = "pokol.mem"
--     Right buf <- createBufferFromFile fn
--       | Left _ => assert_total $ idris_crash "createBufferFromFile"
--     bs <- map cast <$> bufferData buf

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

partial main : IO ()
main = runJS $ do
  -- ui <- initUI

  p <- fetch "http://localhost/po/Makefile"
  p <- p `then_` arrayBuffer
  _ <- p `then_` \buf => (ready () <$) $ do
    pure $ traceConsoleId buf

  pure ()
