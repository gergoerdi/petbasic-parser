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
bits8 x = terminal ("Byte " ++ show x) $ \x' => toMaybe (x == x') ()

letter : Grammar state Bits8 True Bits8
letter = terminal "letter" $ \x => toMaybe (0x41 <= x && x <= 0x5a) x

lexeme : {c : Bool} -> Grammar state Bits8 c a -> Grammar state Bits8 c a
lexeme {c} p = rewrite (lemma c) in {- rewrite (lemma c) in -} p {- <* commit -} <* many (bits8 0x20 <* commit)
  where
    -- lemma : (c : Bool) -> c = (c || Delay False) || Delay False
    lemma : (c : Bool) -> c = c || Delay False
    lemma {c = True} = Refl
    lemma {c = False} = Refl

lineNum : Grammar state Bits8 True LineNum
lineNum = do
  lo <- anyBits8
  hi <- anyBits8
  pure $ cast hi * 256 + cast lo

colon, comma, eq, semi : Grammar state Bits8 True ()
colon = lexeme $ bits8 0x3a <* commit
comma = lexeme $ bits8 0x2c <* commit
eq = lexeme $ bits8 0xb2 <* commit
semi = lexeme $ bits8 0x3b <* commit

parens : {c : Bool} -> Grammar state Bits8 c a -> Grammar state Bits8 True a
parens = between (lexeme $ bits8 0x28 <* commit) (lexeme $ bits8 0x29 <* commit)

digit : Grammar state Bits8 True Bits8
digit = terminal "digit" (\x => toMaybe (0x30 <= x && x <= 0x39) x) <* commit

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
    sign = option False $ True <$ bits8 0xab <* commit

eol : Grammar state Bits8 False ()
eol = ignore $ nextIs "end of line" (== 0x00)

strLit : Grammar state Bits8 True (List Bits8)
strLit = lexeme dquote *> manyTill (dquote <|> eol) anyBits8
  where
    dquote : Grammar state Bits8 True ()
    dquote = bits8 0x22 <* commit

var0 : Grammar state Bits8 True Var0
var0 = lexeme $ do
  b1 <- startId <* commit
  bs <- many (contId <* commit)
  varKind <*> pure (MkId $ b1 ::: bs)
  where
    startId, contId : Grammar state Bits8 True Bits8
    startId = letter
    contId = letter <|> digit

    varKind : Grammar state Bits8 False (Id -> Var0)
    varKind =
          IntVar <$ bits8 0x25 <* commit
      <|> StrVar <$ bits8 0x24 <* commit
      <|> pure RealVar

mutual
  expr : Grammar state Bits8 True Expr
  expr = expressionParser table term <|> fail "expression"
    where
      cmp : Grammar state Bits8 True BinOp
      cmp = choice
        [ Eq  <$ bits8 0xb2 <* commit
        , NEq <$ bits8 0xb3 <* bits8 0xb1 <* commit
        , LE  <$ bits8 0xb3 <* bits8 0xb2 <* commit
        , LT  <$ bits8 0xb3
        , GE  <$ bits8 0xb1 <* bits8 0xb2 <* commit
        , GT  <$ bits8 0xb1
        ]

      table : List (List (Op state Bits8 Expr))
      table =
        [ [ Prefix (lexeme $ NegE <$ bits8 0xab <* commit)
          ]
        , [ Infix (lexeme $ Bin Mul <$ bits8 0xac <* commit) AssocLeft
          ]
        , [ Infix (lexeme $ Bin Plus <$ bits8 0xaa <* commit) AssocLeft
          , Infix (lexeme $ Bin Minus <$ bits8 0xab <* commit) AssocLeft
          ]
        , [ Infix (lexeme $ Bin <$> cmp <* commit) AssocNone ]
        , [ Infix (lexeme $ Bin And <$ bits8 0xaf <* commit) AssocLeft
          , Infix (lexeme $ Bin Or <$ bits8 0xb0 <* commit) AssocLeft
          ]
        ]

      fun : Grammar state Bits8 True Fun
      fun = lexeme $ choice
        [ Peek    <$ bits8 0xc2 <* commit
        , IntFun  <$ bits8 0xb5 <* commit
        , Rnd     <$ bits8 0xbb <* commit
        , Chr     <$ bits8 0xc7 <* commit
        , LeftStr <$ bits8 0xc8 <* commit
        , Val     <$ bits8 0xc5 <* commit
        , Asc     <$ bits8 0xc6 <* commit
        ]

      term : Grammar state Bits8 True Expr
      term =
            NumLitE <$> numLit <* commit
        <|> StrLitE <$> strLit <* commit
        <|> VarE <$> var <* commit
        <|> FunE <$> fun <* commit <*> parens (sepBy1 comma expr) <* commit
        <|> FunE <$> (lexeme $ Tab <$ bits8 0xa3 <* commit) <*> sepBy1 comma expr <* lexeme (bits8 0x29) <* commit
        <|> parens expr

  var : Grammar state Bits8 True Var
  var = MkVar <$> var0 <*> (parens (toList <$> sepBy1 comma expr) <|> pure [])

stmt : Grammar state Bits8 True Stmt
stmt = lexeme $ choice
  [ If <$ bits8 0x8b <* commit <*> expr <* lexeme (bits8 0xa7) <*> (stmt <|> (Goto <$> numLit))
  , Assign <$> var <* eq <*> expr
  , Goto <$ bits8 0x89 <* commit <*> numLit
  , Gosub <$ bits8 0x8d <* commit <*> numLit
  , Return <$ bits8 0x8e <* commit
  , Get <$ bits8 0xa1 <* commit <*> var
  , Poke <$ bits8 0x97 <* commit <*> expr <* comma <*> expr
  , For <$ bits8 0x81 <* commit
        <*> var0 <* eq <*> expr <* lexeme (bits8 0xa4) <*> expr
        <*> lexeme (optional $ bits8 0xa9 *> numLit)
  , Next <$ bits8 0x82 <* commit <*> var0
  , Print <$ bits8 0x99 <* commit <*> many expr <*> (maybe True (const False) <$> optional semi)
  , Clr <$ bits8 0x9c <* commit
  , Run <$ bits8 0x8a <* commit
  , Sys <$ bits8 0x9e <* commit <*> numLit
  , Read <$ bits8 0x87 <* commit <*> var
  , Data <$ bits8 0x83 <* commit <*> sepBy1 comma numLit
  , Open <$ bits8 0x9f <* commit <*> expr <* comma <*> expr <* comma <*> expr <* comma <*> expr
  , Close <$ bits8 0xa0 <* commit <*> expr
  , OnGoto <$ bits8 0x91 <* commit <*> expr <* bits8 0x89 <* commit <*> sepBy1 comma numLit
  , OnGosub <$ bits8 0x91 <* commit <*> expr <* bits8 0x8d <* commit <*> sepBy1 comma numLit
  , PrintH <$ bits8 0x98 <* commit <*> expr <* comma <*> sepBy1 comma expr
  , InputH <$ bits8 0x84 <* commit <*> expr <* comma <*> sepBy1 comma var
  , End <$ bits8 0x80 <* commit
  , Rem <$ bits8 0x8f <* commit <* manyTill eol anyBits8
  ]

line : Grammar state Bits8 True (LineNum, List1 Stmt)
line = (,) <$> lineNum <*> sepBy1 colon stmt <* bits8 0x00 <* anyBits8 <* anyBits8

Show (ParsingError a) where
  show (Error msg _) = msg
