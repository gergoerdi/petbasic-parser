{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict, StrictData #-}
module Parser where

import Prelude hiding (LT, GT)
import Syntax
import Text.Parsec hiding (digit, letter)
import Text.Parsec.Pos
import Text.Parsec.Expr
import Control.Monad
import Text.Printf

type Parser = Parsec [Bits8] ()

terminal :: String -> (Bits8 -> Maybe a) -> Parser a
terminal s f = token (\_ -> s) (\_ -> initialPos "") f

some :: Parser a -> Parser (List1 a)
some = many1

anyBits8 :: Parser Bits8
anyBits8 = terminal "" Just

bits8 :: Bits8 -> Parser ()
bits8 x = terminal (printf "Byte %02x" x) $ \x' -> if x == x' then Just () else Nothing

letter :: Parser Bits8
letter = terminal "letter" $ \x -> guard (0x41 <= x && x <= 0x5a) >> pure x

lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany (bits8 0x20)

lineNum :: Parser LineNum
lineNum = do
  lo <- anyBits8
  hi <- anyBits8
  pure $ fromIntegral hi * 256 + fromIntegral lo

colon, comma, eq, semi :: Parser ()
colon = lexeme $ bits8 0x3a
comma = lexeme $ bits8 0x2c
eq = lexeme $ bits8 0xb2
semi = lexeme $ bits8 0x3b

parens :: Parser a -> Parser a
parens = between (lexeme $ bits8 0x28) (lexeme $ bits8 0x29)

digit :: Parser Bits8
digit = terminal "digit" $ \x -> guard (0x30 <= x && x <= 0x39) >> pure x

digitLit :: (Num a) => Parser a
digitLit = fromInteger . fromIntegral . (\x -> x - 0x30) <$> digit

numLit :: forall a. (Num a) => Parser a
numLit = fromDigits <$> lexeme sign <*> lexeme (some digitLit)
  where
    fromDigits :: Bool -> List1 a -> a
    fromDigits neg =
      (if neg then negate else id) .
      foldl (\x y -> x * 10 + y) 0

    sign :: Parser Bool
    sign = option False $ True <$ bits8 0xab

eol :: Parser ()
eol = lookAhead $ bits8 0x00

strLit :: Parser (List Bits8)
strLit = lexeme dquote *> flip manyTill (dquote <|> eol) anyBits8
  where
    dquote :: Parser ()
    dquote = bits8 0x22

var0 :: Parser Var0
var0 = do
  b1 <- lexeme startId
  bs <- many contId
  varKind <*> pure (MkId $ b1 : bs)
  where
    startId, contId :: Parser Bits8
    startId = letter
    contId = letter <|> digit

    varKind :: Parser (Id -> Var0)
    varKind =
          IntVar <$ bits8 0x25
      <|> StrVar <$ bits8 0x24
      <|> pure RealVar

expr :: Parser Expr
expr = buildExpressionParser table term <|> fail "expression"
  where
    cmp :: Parser BinOp
    cmp = choice
      [ Eq  <$ bits8 0xb2
      , NEq <$ try (bits8 0xb3 <* bits8 0xb1)
      , GE  <$ try (bits8 0xb1 <* bits8 0xb2)
      , GT  <$ bits8 0xb1
      , LE  <$ try (bits8 0xb3 <* bits8 0xb2)
      , LT  <$ bits8 0xb3
      ]

    table =
      [ [ Prefix (lexeme $ NegE <$ bits8 0xab)
        ]
      , [ Infix (lexeme $ Bin Mul <$ bits8 0xac) AssocLeft
        ]
      , [ Infix (lexeme $ Bin Plus <$ bits8 0xaa) AssocLeft
        , Infix (lexeme $ Bin Minus <$ bits8 0xab) AssocLeft
        ]
      , [ Infix (Bin <$> lexeme cmp) AssocNone ]
      , [ Infix (lexeme $ Bin And <$ bits8 0xaf) AssocLeft
        , Infix (lexeme $ Bin Or <$ bits8 0xb0) AssocLeft
        ]
      ]

    fun :: Parser Fun
    fun = lexeme $ choice
      [ Peek    <$ bits8 0xc2
      , IntFun  <$ bits8 0xb5
      , Rnd     <$ bits8 0xbb
      , Chr     <$ bits8 0xc7
      , LeftStr <$ bits8 0xc8
      , Val     <$ bits8 0xc5
      , Asc     <$ bits8 0xc6
      ]

    term :: Parser Expr
    term =
          NumLitE <$> numLit
      <|> StrLitE <$> strLit
      <|> VarE <$> var
      <|> FunE <$> fun <*> parens (flip sepBy1 comma expr)
      <|> FunE <$> (lexeme $ Tab <$ bits8 0xa3) <*> flip sepBy1 comma expr <* lexeme (bits8 0x29)
      <|> parens expr

var :: Parser Var
var = MkVar <$> var0 <*> (parens (flip sepBy1 comma expr) <|> pure [])

stmt :: Parser Stmt
stmt = lexeme $ choice
  [ If <$ bits8 0x8b <*> expr <* lexeme (bits8 0xa7) <*> (stmt <|> (Goto <$> numLit))
  , Assign <$> var <* eq <*> expr
  , Goto <$ bits8 0x89 <*> numLit
  , Gosub <$ bits8 0x8d <*> numLit
  , Return <$ bits8 0x8e
  , Get <$ bits8 0xa1 <*> var
  , Poke <$ bits8 0x97 <*> expr <* comma <*> expr
  , For <$ bits8 0x81
        <*> var0 <* eq <*> expr <* lexeme (bits8 0xa4) <*> expr
        <*> lexeme (optionMaybe $ bits8 0xa9 *> numLit)
  , Next <$ bits8 0x82 <*> var0
  , Print <$ bits8 0x99 <*> many expr <*> (maybe True (const False) <$> optionMaybe semi)
  , Clr <$ bits8 0x9c
  , Run <$ bits8 0x8a
  , Sys <$ bits8 0x9e <*> numLit
  , Read <$ bits8 0x87 <*> var
  , Data <$ bits8 0x83 <*> flip sepBy1 comma numLit
  , Open <$ bits8 0x9f <*> expr <* comma <*> expr <* comma <*> expr <* comma <*> expr
  , Close <$ bits8 0xa0 <*> expr
  , OnGoto <$ bits8 0x91 <*> expr <* bits8 0x89 <*> flip sepBy1 comma numLit
  , OnGosub <$ bits8 0x91 <*> expr <* bits8 0x8d <*> flip sepBy1 comma numLit
  , PrintH <$ bits8 0x98 <*> expr <* comma <*> flip sepBy1 comma expr
  , InputH <$ bits8 0x84 <*> expr <* comma <*> flip sepBy1 comma var
  , End <$ bits8 0x80
  , Rem <$ bits8 0x8f <* flip manyTill eol anyBits8
  ]

line :: Parser (LineNum, List1 Stmt)
line = (,) <$> lineNum <*> flip sepBy1 colon stmt <* bits8 0x00 <* anyBits8 <* anyBits8
