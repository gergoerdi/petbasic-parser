module Parser.Test

import Syntax
import Parser

import Text.Parser
import Data.Buffer
import System.File.Buffer
import Data.List
import Data.Maybe
import System

loadImage : String -> IO (List Bits8)
loadImage fn = do
    Right buf <- createBufferFromFile fn
      | Left _ => pure []
    map (map cast) $ bufferData buf

replicateM : (n : Nat) -> Grammar state k True a -> Grammar state k (n > 0) (List a)
replicateM 0 _ = pure []
replicateM (S n) act = (::) <$> act <*> replicateM n act

main : IO ()
main = do
    n <- getArgs >>= \xs => pure $ case xs of
      [_, x] => cast x
      _ => 1148

    let fn = "pokol.mem"
    buf <- loadImage fn
    let buf = drop 2 buf
    let buf = let (pre, post) = splitAt (0x0803 + 28282) buf
              in  pre ++ [0x99] ++ post
    let buf = drop 0x0803 buf
    case parse (replicateM n line) $ map irrelevantBounds buf of
      Left (err1 ::: errs) => printLn err1 >> printLn errs
      Right (x, rest) => traverse_ (printLn . fst) x
