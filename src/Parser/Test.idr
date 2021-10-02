module Parser.Test

import Syntax
import Parser

import Text.Lexer
import Text.Parser
import Data.Buffer
import System.File.Buffer
import Data.List
import Data.Maybe

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
    let fn = "pokol.mem"
    buf <- loadImage fn
    let buf = drop 2 buf
    let buf = let (pre, post) = splitAt (0x0803 + 28285) buf
              in  pre ++ [0xb2] ++ post
    let buf = drop 0x0803 buf
    case parse (replicateM 9 line) $ map irrelevantBounds buf of
      Left (err1 ::: errs) => printLn err1 >> printLn errs
      Right (x, rest) => traverse_ printLn x >> printLn (map val $ take 20 rest)
