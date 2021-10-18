import Syntax
import Parser
import Interpreter

import Text.Parser
import Data.Buffer
import System.File.Buffer
import Data.List
import Data.Maybe
import Data.String
import System

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Maybe
import Control.Monad.Either

replicateM : (n : Nat) -> Grammar st k True a -> Grammar st k (n > 0) (List a)
replicateM 0 _ = pure []
replicateM (S n) act = (::) <$> act <*> replicateM n act

replicateM_ : Applicative m => (n : Nat) -> m a -> m ()
replicateM_ 0 _ = pure ()
replicateM_ (S n) act = act *> replicateM_ n act

partial forever : Monad m => m () -> m b
forever f = f >> forever f

loadGame : IO (List (LineNum, List1 Stmt))
loadGame = do
    let fn = "pokol.mem"
    Right buf <- createBufferFromFile fn
      | Left _ => assert_total $ idris_crash "createBufferFromFile"
    bs <- map cast <$> bufferData buf
    let bs = drop 2 bs
    let bs = let (pre, post) = splitAt (0x0803 + 28282) bs
              in  pre ++ [0x99] ++ post
    let bs = drop 0x0803 bs
    case parse (replicateM 1148 line) . map irrelevantBounds $ bs of
      Left errs => assert_total $ idris_crash "parse"
      Right (x, rest) => pure x

partial main : IO ()
main = do
  lines <- loadGame
  let lines = sortBy (comparing fst) lines
  -- traverse_ printLn lines

  let (r, s) = startBASIC lines
  let loop : S -> BASIC () -> IO ()
      loop s act = do
        let (s', out) = runBASIC r s act
        printLn out
        next <- case out of
          WaitInput actions => do
            let waitInput : IO (BASIC ())
                waitInput = do
                  putStr "> "
                  s <- toLower <$> getLine
                  case words s of
                    ["do", n] => pure $ playerAction $ cast n
                    ["go", n] => pure $ playerMove $ cast n
                    _ => waitInput
            waitInput
          _ => pure execLine
        loop s' next
  loop s execLine
