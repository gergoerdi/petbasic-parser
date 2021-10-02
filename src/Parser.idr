module Parser

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

lexeme : {c : Bool} -> Grammar state Bits8 c a -> Grammar state Bits8 c a
lexeme p = afterMany (bits8 0x20) p

LineNum : Type
LineNum = Bits16

lineNum : Grammar state Bits8 True LineNum
lineNum = do
  lo <- anyBits8
  hi <- anyBits8
  pure $ cast hi * 256 + cast lo

data Expr = NumLitE Double

Show Expr where
  show (NumLitE n) = show n

data Stmt = Goto LineNum | Poke Expr Expr

Show Stmt where
  show (Poke addr dat) = "POKE " ++ show addr ++ ", " ++ show dat
  show (Goto n) = "GOTO " ++ show n

colon : Grammar state Bits8 True ()
colon = lexeme $ bits8 0x3a

comma : Grammar state Bits8 True ()
comma = lexeme $ bits8 0x2c

digit : (Num a) => Grammar state Bits8 True a
digit = terminal "literal digit" $ \x =>
  toMaybe (0x30 <= x && x <= 0x39) $ 
  fromInteger . cast $ x - 0x30

numLit : (Num a, Neg a) => Grammar state Bits8 True a
numLit {a} = fromDigits <$> sign <*> lexeme (some digit)
  where
    fromDigits : Bool -> List1 a -> a
    fromDigits neg =
      (if neg then negate else id) .
      foldl (\x => \y => x * 10 + y) (the a 0)

    sign : Grammar state Bits8 False Bool
    sign = option False $ True <$ bits8 0xab


expr : Grammar state Bits8 True Expr
expr = NumLitE <$> numLit

stmt : Grammar state Bits8 True Stmt
stmt = lexeme $ do
  Poke <$ bits8 0x97 <*> expr <* comma <*> expr <|>
  Goto <$ bits8 0x89 <*> numLit

line : Grammar state Bits8 True (LineNum, List1 Stmt)
line = (,) <$> lineNum <*> sepBy1 colon stmt <* bits8 0x00 <* anyBits8 <* anyBits8

Show (ParsingError a) where
  show (Error msg _) = msg

main : IO ()
main = do
    let fn = "pokol.mem"
    buf <- loadImage fn
    let buf = drop 2 buf
    let buf = let (pre, post) = splitAt (0x0803 + 28285) buf
              in  pre ++ [0xb2] ++ post
    let buf = drop 0x0803 buf
    case parse (lineNum *> stmt <* colon) $ map irrelevantBounds buf of
      Left (err1 ::: errs) => printLn err1 >> printLn errs
      Right (x, rest) => printLn x >> printLn (map val $ take 10 rest)
