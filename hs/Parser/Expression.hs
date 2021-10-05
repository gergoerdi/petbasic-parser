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

        -- sortedOps :: ( [Parsec k state (a -> a -> a)]
        --              , [Parsec k state (a -> a -> a)]
        --              , [Parsec k state (a -> a -> a)]
        --              , [Parsec k state (a -> a)]
        --              )
        sortedOps = foldr separate ([], [], [], []) ops

        parseThese =
          let (lassoc, rassoc, nassoc, pre) = sortedOps
              parseLeft = do
                x <- factor
                fs <- some (flip <$> choice lassoc <*> factor)
                pure $ foldl (flip ($)) x fs
              parseRight = case rassoc of
                  [] -> fail "right-associative operator"
                  (r:rs) -> do
                      fs <- some (factor >>= \x -> choice1 r rs <*> pure x)
                      y <- factor
                      pure $ foldr ($) y fs
              parseNone = case nassoc of
                  [] -> fail "non-associative operator"
                  (n:ns) -> do
                      x <- factor
                      f <- choice1 n ns
                      y <- factor
                      pure $ f x y
              parsePre = case pre of
                  [] -> fail "prefix operator"
                  (p:ps) -> do
                      choice1 p ps <*> factor
          in parseLeft <|> parseRight <|> parseNone <|> parsePre
