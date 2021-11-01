module Pokol.PrepareText

import Pokol.Text

import Data.String
import Data.List
import Data.Buffer
import Language.JSON

import System
import System.File.Buffer
import System.Path

textFromBuf : List Bits8 -> List String
textFromBuf = take 7 . filter (not . null) . lines . pack . map readable

getBits8List : HasIO io => Buffer -> io (List Bits8)
getBits8List buf = do
  n <- rawSize buf
  go n 0
  where
    go : Int -> Int -> io (List Bits8)
    go n i = if i >= n then pure [] else [| getBits8 buf i :: go n (i + 1) |]

toJSON1 : (String, List String) -> String
toJSON1 (k, v) = #"  "\#{k}":\#n    [\#{arr v}"#
  where
  arr : List String -> String
  arr [] = "]"
  arr [s] = #""\#{s}"]"#
  arr (s::ss) = #""\#{s}",\#n     \#{arr ss}"#

toJSON : List (String, List String) -> String
toJSON = unlines . (["{"] <+>) . comma . map toJSON1
  where
    comma : List String -> List String
    comma [] = ["}"]
    comma [s] = s :: comma []
    comma (s::ss) = (s <+> ",") :: comma ss

main : IO ()
main = do
  (_ :: args) <- getArgs
    | _ => exitFailure
  kvs <- for args $ \fn => do
    Just fileKey <- pure $ fileName fn
      | Nothing => printLn fn *> exitFailure
    Right buf <- createBufferFromFile fn
      | Left err => printLn err *> exitFailure
    s <- textFromBuf . drop 2 <$> getBits8List buf
    pure (fileKey, s)
  putStrLn "let texts = "
  putStr $ toJSON kvs
  putStrLn ";"
