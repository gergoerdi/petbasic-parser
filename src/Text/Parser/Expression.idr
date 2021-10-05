module Text.Parser.Expression

import Text.Parser
import Data.List1

public export
data Assoc
   = AssocNone
   | AssocLeft
   | AssocRight

public export
data Op state k a
  = Prefix (Grammar state k True (a -> a))
  | Infix (Grammar state k True (a -> a -> a)) Assoc

public export
OpTable : Type -> Type -> Type -> Type
OpTable state k a = List (List (Op state k a))

public export
expressionParser :
  OpTable state k a ->
  Grammar state k True a ->
  Grammar state k True a
expressionParser table term = foldl level term table
  where
    level : Grammar state k True a -> List (Op state k a) -> Grammar state k True a
    level factor ops = parseThese <|> factor
      where
        0 BinOp, UnOp : Type
        BinOp = Grammar state k True (a -> a -> a)
        UnOp = Grammar state k True (a -> a)

        0 SortedOps : Type
        SortedOps = (List BinOp, List BinOp, List BinOp, List UnOp)

        separate : Op state k a -> SortedOps -> SortedOps
        separate op (lassoc, rassoc, nassoc, pre) = case op of
          Infix p AssocLeft  => (p::lassoc, rassoc, nassoc, pre)
          Infix p AssocRight => (lassoc, p::rassoc, nassoc, pre)
          Infix p AssocNone  => (lassoc, rassoc, p::nassoc, pre)
          Prefix p           => (lassoc, rassoc, nassoc, p::pre)

        sortedOps : SortedOps
        sortedOps = foldr separate ([], [], [], []) ops

        parseThese : Grammar state k True a
        parseThese =
          let (lassoc, rassoc, nassoc, pre) = sortedOps

              termP : Grammar state k True a
              prefixP : Grammar state k False (a -> a)
              termP = do
                  f <- prefixP
                  x <- factor
                  pure $ f x

              prefixP = choice pre <|> pure id

              rassocP : a -> Grammar state k True a
              rassocP1 : a -> Grammar state k False a
              rassocP x = do
                  f <- choice rassoc
                  y <- termP >>= rassocP1
                  pure $ f x y
              rassocP1 x = rassocP x <|> pure x

              lassocP : a -> Grammar state k True a
              lassocP1 : a -> Grammar state k False a
              lassocP x = do
                  f <- choice lassoc
                  y <- termP
                  lassocP1 $ f x y
              lassocP1 x = lassocP x <|> pure x

              nassocP : a -> Grammar state k True a
              nassocP x = do
                  f <- choice nassoc
                  y <- termP
                  pure $ f x y

          in do
              x <- termP
              rassocP x <|> lassocP x <|> nassocP x <|> pure x
