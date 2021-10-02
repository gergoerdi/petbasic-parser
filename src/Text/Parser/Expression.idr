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
expressionParser :
  List (List (Op state k a)) ->
  Grammar state k True a ->
  Grammar state k True a
expressionParser table term = foldr level term table
  where
    level : List (Op state k a) -> Grammar state k True a -> Grammar state k True a
    level ops factor = choiceMap op ops <|> factor
      where
        op : Op state k a -> Grammar state k True a
        op (Infix parser assoc) = do -- TODO: associativity
          x <- factor
          f <- parser
          y <- factor
          pure $ f x y
        op (Prefix parser) = parser <*> factor
