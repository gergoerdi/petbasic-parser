{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict, StrictData #-}
module Parser.Compat where

import Data.Word
import Text.Parsec

type Parser = Parsec [Bits8] ()

some :: Parser a -> Parser (List1 a)
some = many1

type Bits8 = Word8

type Bits16 = Word16
type List = ([])
type List1 = List
