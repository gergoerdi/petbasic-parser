module PrepareBinary

import PETBASIC.Syntax
import PETBASIC.Parser
import PETBASIC.Binary

import Text.Parser as P
import Data.List

import Data.Binary
import Data.Binary.Buffer

import Data.Buffer
import System.File
import System.File.Buffer

import Control.Monad.Reader
import Control.Monad.State

replicateM : (n : Nat) -> Grammar st k True a -> Grammar st k (n > 0) (List a)
replicateM 0 _ = pure []
replicateM (S n) act = (::) <$> act <*> replicateM n act

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

main : IO ()
main = do
  lines <- loadGame

  Just buf <- newBuffer 60000
    | Nothing => pure ()
  len <- execStateT 0 $ runReaderT buf $ putList putLine lines
  printLn len

  Right () <- writeBufferToFile "pokol.ppb" buf len
    | Left err => pure ()
  pure ()
  where
    putLine : (LineNum, List1 Stmt) -> Put ()
    putLine (lineNum, stmts) = put lineNum *> putList1 put stmts
