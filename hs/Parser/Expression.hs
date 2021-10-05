{-# LANGUAGE ScopedTypeVariables #-}
module Parser.Expression where

import Parser.Compat
import Text.Parsec

data Assoc
   = AssocNone
   | AssocLeft
   | AssocRight

data Op state k a
  = Prefix (Parsec k state (a -> a))
  | Infix (Parsec k state (a -> a -> a)) Assoc

type OpTable state k a = List (List (Op state k a))

choice1 p0 ps = go ps
  where
    go [] = p0
    go (p:ps) = p <|> go ps

expressionParser :: forall a. OpTable () [Bits8] a -> Parser a -> Parser a
expressionParser table term = foldl level term table
  where
    level factor ops = parseThese <|> factor
      where
        separate op (lassoc, rassoc, nassoc, pre) = case op of
          Infix p AssocLeft  -> (p:lassoc, rassoc, nassoc, pre)
          Infix p AssocRight -> (lassoc, p:rassoc, nassoc, pre)
          Infix p AssocNone  -> (lassoc, rassoc, p:nassoc, pre)
          Prefix p           -> (lassoc, rassoc, nassoc, p:pre)

        sortedOps = foldr separate ([], [], [], []) ops

        parseThese =
          let (lassoc, rassoc, nassoc, pre) = sortedOps

              termP :: Parser a
              termP = do
                  pre <- prefixP
                  x <- factor
                  pure $ pre x

              prefixP = choice pre <|> pure id

              rassocP x = do
                  f <- choice rassoc
                  y <- termP >>= rassocP1
                  pure $ f x y
              rassocP1 x = rassocP x <|> pure x

              lassocP x = do
                  f <- choice lassoc
                  y <- termP
                  lassocP1 $ f x y
              lassocP1 x = lassocP x <|> pure x

              nassocP x = do
                  f <- choice nassoc
                  y <- termP
                  pure $ f x y

          in do
              x <- termP
              rassocP x <|> lassocP x <|> nassocP x <|> pure x
