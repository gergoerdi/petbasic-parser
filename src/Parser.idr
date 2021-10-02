module Parser

import Syntax
import Text.Lexer
import Text.Parser
import Text.Parser.Expression
import Data.List
import Data.Maybe

anyBits8 : Grammar state Bits8 True Bits8
anyBits8 = terminal "" Just

bits8 : Bits8 -> Grammar state Bits8 True ()
bits8 x = terminal ("Byte " ++ show x) $ \x' => if x == x' then Just () else Nothing

letter : Grammar state Bits8 True Bits8
letter = terminal "letter" $ \x => toMaybe (0x41 <= x && x <= 0x5a) x

lexeme : {c : Bool} -> Grammar state Bits8 c a -> Grammar state Bits8 c a
lexeme p = afterMany (bits8 0x20) p

lineNum : Grammar state Bits8 True LineNum
lineNum = do
  lo <- anyBits8
  hi <- anyBits8
  pure $ cast hi * 256 + cast lo

colon, comma, eq, semi : Grammar state Bits8 True ()
colon = lexeme $ bits8 0x3a
comma = lexeme $ bits8 0x2c
eq = lexeme $ bits8 0xb2
semi = lexeme $ bits8 0x3b

parens : {c : Bool} -> Grammar state Bits8 c a -> Grammar state Bits8 True a
parens = between (lexeme $ bits8 0x28) (lexeme $ bits8 0x29)

digit : Grammar state Bits8 True Bits8
digit = terminal "digit" $ \x =>
  toMaybe (0x30 <= x && x <= 0x39) x

digitLit : (Num a) => Grammar state Bits8 True a
digitLit = fromInteger . cast . (\x => x - 0x30) <$> digit

numLit : (Num a, Neg a) => Grammar state Bits8 True a
numLit {a} = fromDigits <$> lexeme sign <*> lexeme (some digitLit)
  where
    fromDigits : Bool -> List1 a -> a
    fromDigits neg =
      (if neg then negate else id) .
      foldl (\x => \y => x * 10 + y) (the a 0)

    sign : Grammar state Bits8 False Bool
    sign = option False $ True <$ bits8 0xab

strLit : Grammar state Bits8 True (List Bits8)
strLit = lexeme dquote *> manyTill dquote anyBits8
  where
    dquote : Grammar state Bits8 True ()
    dquote = bits8 0x22

var0 : Grammar state Bits8 True Var0
var0 = do
  b1 <- lexeme startId
  bs <- many contId
  varKind <*> pure (MkId $ b1 ::: bs)
  where
    startId, contId : Grammar state Bits8 True Bits8
    startId = letter
    contId = letter <|> digit

    varKind : Grammar state Bits8 False (Id -> Var0)
    varKind =
          IntVar <$ bits8 0x25
      <|> StrVar <$ bits8 0x24
      <|> pure RealVar

mutual
  expr : Grammar state Bits8 True Expr
  expr = expressionParser table term <|> fail "expression"
    where
      bin : Bits8 -> (a -> a -> b) -> (Grammar state Bits8 True a) -> Grammar state Bits8 True b
      bin op f tm = f <$> tm <*> (lexeme (bits8 op) *> tm)

      table : List (List (Op state Bits8 Expr))
      table =
        [ [ Prefix (lexeme $ NegE <$ bits8 0xab)
          ]
        , [ Infix (lexeme $ Bin Mul <$ bits8 0xac) AssocLeft
          ]
        , [ Infix (lexeme $ Bin Plus <$ bits8 0xaa) AssocLeft
          , Infix (lexeme $ Bin Minus <$ bits8 0xab) AssocLeft
          ]
        , [ Infix (lexeme $ Bin Eq  <$ bits8 0xb2) AssocNone
          , Infix (lexeme $ Bin NEq <$ (bits8 0xb3 *> bits8 0xb1)) AssocNone
          , Infix (lexeme $ Bin GE  <$ (bits8 0xb1 *> bits8 0xb2)) AssocNone
          , Infix (lexeme $ Bin GT  <$ bits8 0xb1) AssocNone
          , Infix (lexeme $ Bin LE  <$ (bits8 0xb3 *> bits8 0xb2)) AssocNone
          , Infix (lexeme $ Bin LT  <$ bits8 0xb3) AssocNone
          ]
        , [ Infix (lexeme $ Bin And <$ bits8 0xaf) AssocLeft
          , Infix (lexeme $ Bin Or <$ bits8 0xb0) AssocLeft
          ]
        ]

      fun : Grammar state Bits8 True Fun
      fun = lexeme $ choice
        [ Peek    <$ bits8 0xc2
        , IntFun  <$ bits8 0xb5
        , Rnd     <$ bits8 0xbb
        , Chr     <$ bits8 0xc7
        , LeftStr <$ bits8 0xc8
        , Val     <$ bits8 0xc5
        , Asc     <$ bits8 0xc6
        ]

      term : Grammar state Bits8 True Expr
      term =
            NumLitE <$> numLit
        <|> StrLitE <$> strLit
        <|> VarE <$> var
        <|> FunE <$> fun <*> parens (sepBy1 comma expr)
        <|> parens expr

  var : Grammar state Bits8 True Var
  var = MkVar <$> var0 <*> (parens (toList <$> sepBy1 comma expr) <|> pure [])

stmt : Grammar state Bits8 True Stmt
stmt = lexeme $ choice
  [ If <$ bits8 0x8b <*> expr <* lexeme (bits8 0xa7) <*> (stmt <|> (Goto <$> numLit))
  , Assign <$> var <* eq <*> expr
  , Goto <$ bits8 0x89 <*> numLit
  , Gosub <$ bits8 0x8d <*> numLit
  , Poke <$ bits8 0x97 <*> expr <* comma <*> expr
  , For <$ bits8 0x81
        <*> var0 <* eq <*> expr <* lexeme (bits8 0xa4) <*> expr
        <*> lexeme (optional $ bits8 0xa9 *> numLit)
  , Next <$ bits8 0x82 <*> var0
  , Print <$ bits8 0x99 <*> many expr <*> (maybe True (const False) <$> optional semi)
  , Clr <$ bits8 0x9c
  , Run <$ bits8 0x8a
  , Sys <$ bits8 0x9e <*> numLit
  , Read <$ bits8 0x87 <*> var
  , Data <$ bits8 0x83 <*> sepBy1 comma numLit
  , Open <$ bits8 0x9f <*> expr <* comma <*> expr <* comma <*> expr <* comma <*> expr
  , Close <$ bits8 0xa0 <*> expr
  , OnGoto <$ bits8 0x91 <*> expr <* bits8 0x89 <*> sepBy1 comma numLit
  , OnGosub <$ bits8 0x91 <*> expr <* bits8 0x8d <*> sepBy1 comma numLit
  , PrintH <$ bits8 0x98 <*> expr <* comma <*> sepBy1 comma expr
  , InputH <$ bits8 0x84 <*> expr <* comma <*> sepBy1 comma var
  ]

line : Grammar state Bits8 True (LineNum, List1 Stmt)
line = (,) <$> lineNum <*> sepBy1 colon stmt <* bits8 0x00 <* anyBits8 <* anyBits8

Show (ParsingError a) where
  show (Error msg _) = msg
