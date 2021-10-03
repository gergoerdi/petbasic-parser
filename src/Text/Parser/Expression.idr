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

choice1 : List1 (Grammar state k True a) -> Grammar state k True a
choice1 (p ::: ps) = p <|> choice ps

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
              parseLeft = do
                x <- factor
                fs <- some (flip <$> choice lassoc <*> factor)
                pure $ foldl (flip ($)) x fs
              parseRight = case fromList rassoc of
                Nothing => fail "right-associative operator"
                Just rassoc => do
                  fs <- some (factor >>= \x => choice1 rassoc <*> pure x)
                  y <- factor
                  pure $ foldr ($) y fs
              parseNone = case fromList nassoc of
                Nothing => fail "non-associative operator"
                Just nassoc => do
                  x <- factor
                  f <- choice1 nassoc
                  y <- factor
                  pure $ f x y
              parsePre = case fromList pre of
                Nothing => fail "prefix operator"
                Just pre => do
                  choice1 pre <*> factor
          in parseLeft <|> parseRight <|> parseNone <|> parsePre
