module Parser

import Syntax
import Text.Lexer
import Text.Parser
import Data.Buffer
import System.File.Buffer
import Data.List
import Data.Maybe

loadImage : String -> IO (List Bits8)
loadImage fn = do
    Right buf <- createBufferFromFile fn
      | Left _ => pure []
    map (map cast) $ bufferData buf

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

colon, comma, eq : Grammar state Bits8 True ()
colon = lexeme $ bits8 0x3a
comma = lexeme $ bits8 0x2c
eq = lexeme $ bits8 0xb2

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
      IntVar <$ bits8 0x25 <|>
      StrVar <$ bits8 0x24 <|>
      pure RealVar

data Assoc
   = AssocNone
   | AssocLeft
   | AssocRight

data Op state k a
  = Prefix (Grammar state k True (a -> a))
  | Infix (Grammar state k True (a -> a -> a)) Assoc

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

mutual
  expr : Grammar state Bits8 True Expr
  expr = expressionParser table term
    where
      bin : Bits8 -> (a -> a -> b) -> (Grammar state Bits8 True a) -> Grammar state Bits8 True b
      bin op f tm = f <$> tm <*> (lexeme (bits8 op) *> tm)

      table : List (List (Op state Bits8 Expr))
      table =
        [ [ Infix (lexeme $ PlusE <$ bits8 0xaa) AssocLeft
          ]
        , [ Infix (lexeme $ AndE <$ bits8 0xaf) AssocLeft
          , Infix (lexeme $ OrE <$ bits8 0xb0) AssocLeft
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
        <|> VarE <$> var
        <|> FunE <$> fun <*> parens (sepBy1 comma expr)
        <|> parens expr

  var : Grammar state Bits8 True Var
  var = MkVar <$> var0 <*> ({-parens (sepBy1 comma expr) <|> -} pure [])

stmt : Grammar state Bits8 True Stmt
stmt = lexeme $ choice
  [ Poke <$ bits8 0x97 <*> expr <* comma <*> expr
  , Goto <$ bits8 0x89 <*> numLit
  , For <$ bits8 0x81
        <*> var0 <* eq <*> expr <* lexeme (bits8 0xa4) <*> expr
        <*> lexeme (optional $ bits8 0xa9 *> numLit)
  , Next <$ bits8 0x82 <*> var0
  , Read <$ bits8 0x87 <*> var
  , Data <$ bits8 0x83 <*> sepBy1 comma numLit
  ]

line : Grammar state Bits8 True (LineNum, List1 Stmt)
line = (,) <$> lineNum <*> sepBy1 colon stmt <* bits8 0x00 <* anyBits8 <* anyBits8

Show (ParsingError a) where
  show (Error msg _) = msg

replicateM : (n : Nat) -> Grammar state k True a -> Grammar state k (n > 0) (List a)
replicateM 0 _ = pure []
replicateM (S n) act = (::) <$> act <*> replicateM n act

main : IO ()
main = do
    let fn = "pokol.mem"
    buf <- loadImage fn
    let buf = drop 2 buf
    let buf = let (pre, post) = splitAt (0x0803 + 28285) buf
              in  pre ++ [0xb2] ++ post
    let buf = drop 0x0803 buf
    case parse (replicateM 6 line) $ map irrelevantBounds buf of
      Left (err1 ::: errs) => printLn err1 >> printLn errs
      Right (x, rest) => traverse_ printLn x >> printLn (map val $ take 20 rest)
