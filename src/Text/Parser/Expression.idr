module Text.Parser.Expression

import Text.Parser

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
    level factor ops = choiceMap op ops <|> factor
      where
        op : Op state k a -> Grammar state k True a
        op (Infix parser AssocNone) = do
          x <- factor
          f <- parser
          y <- factor
          pure $ f x y
        op (Infix parser AssocLeft) = do
          x <- factor
          fs <- some (flip <$> parser <*> factor)
          pure $ foldl (flip ($)) x fs
        op (Infix parser AssocRight) = do
          fs <- some (factor >>= \x => parser <*> pure x)
          y <- factor
          pure $ foldr ($) y fs
        op (Prefix parser) = parser <*> factor
