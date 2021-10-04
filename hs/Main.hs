module Main where

import Syntax
import Parser
import Text.Parsec
import Control.Monad
import qualified Data.ByteString as BS

main :: IO ()
main = do
    buf <- BS.unpack <$> BS.readFile "../pokol.mem"
    buf <- return $ drop 2 buf
    buf <- return $ let (pre, post) = splitAt (0x0803 + 28282) buf
                    in  pre ++ [0x99] ++ post
    buf <- return $ drop 0x0803 buf
    case parse (replicateM 1148 line) "" buf of
        Left err -> print err
        Right x -> mapM_ (print . fst) x
