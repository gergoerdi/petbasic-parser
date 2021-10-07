module Syntax

import Data.List1
import Data.String

public export
LineNum : Type
LineNum = Bits16

public export
Number : Type
Number = Double

public export
data Id = MkId (List1 Bits8)

public export
implementation Eq Id where
  MkId bs == MkId bs' = bs == bs'

public export
implementation Ord Id where
  compare (MkId bs) (MkId bs') = compare bs bs'

public export
data Var0
  = RealVar Id
  | IntVar Id
  | StrVar Id

public export
implementation Eq Var0 where
  RealVar nm == RealVar nm' = nm == nm'
  IntVar nm == IntVar nm'   = nm == nm'
  StrVar nm == StrVar nm'   = nm == nm'
  _ == _ = False

public export
implementation Ord Var0 where
  compare (RealVar nm) (RealVar nm') = compare nm nm'
  compare (RealVar nm) _ = LT
  compare (IntVar nm) (StrVar _) = GT
  compare (IntVar nm) (IntVar nm') = compare nm nm'
  compare (IntVar nm) _ = LT
  compare (StrVar nm) (RealVar _) = GT
  compare (StrVar nm) (IntVar _) = GT
  compare (StrVar nm) (StrVar nm') = compare nm nm'

public export
data Fun
  = Peek
  | IntFun
  | Rnd
  | LeftStr
  | Chr
  | Val
  | Asc
  | Tab

public export
data BinOp
  = Eq
  | NEq
  | LT
  | LE
  | GT
  | GE
  | Plus
  | Minus
  | Mul
  | And
  | Or

mutual
  public export
  data Var
    = MkVar Var0 (List Expr)

  public export
  data Expr
    = VarE Var
    | StrLitE (List Bits8)
    | NumLitE Number
    | Bin BinOp Expr Expr
    | FunE Fun (List1 Expr)
    | NegE Expr

public export
data Stmt
  = If Expr Stmt
  | Assign Var Expr
  | Goto LineNum
  | Gosub LineNum
  | Return
  | Poke Expr Expr
  | For Var0 Expr Expr (Maybe Number)
  | Read Var
  | Next Var0
  | Data (List1 Number)
  | Print (List Expr) Bool
  | PrintH Expr (List1 Expr)
  | Clr
  | Run
  | Sys Bits16
  | Open Expr Expr Expr Expr
  | InputH Expr (List1 Var)
  | Close Expr
  | OnGoto Expr (List1 LineNum)
  | OnGosub Expr (List1 LineNum)
  | Get Var
  | End
  | Rem

readable : (Foldable f) => f Bits8 -> String
readable = concat . map toChar . toList
  where
    toChar : Bits8 -> String
    toChar x = if 0x20 <= x && x <= 0x7f then cast . chr . cast $ x
           else "{" ++ show x ++ "}"

public export
Show Id where
  show (MkId nm) = readable nm

public export
Show Var0 where
  show (RealVar nm) = show nm
  show (IntVar nm) = show nm ++ "%"
  show (StrVar nm) = show nm ++ "$"

public export
Show Fun where
  show Peek = "PEEK"
  show IntFun = "INT"
  show Rnd = "RND"
  show LeftStr = "LEFT$"
  show Chr = "CHR$"
  show Val = "VAL"
  show Asc = "ASC"
  show Tab = "TAB"

public export
Show BinOp where
  show Eq = "="
  show NEq = "<>"
  show LT = "<"
  show LE = "<="
  show GT = ">"
  show GE = ">="
  show Plus = "+"
  show Minus = "-"
  show Mul = "*"
  show And = "AND"
  show Or = "OR"

intersperse : String -> List String -> String
intersperse sep [] = ""
intersperse sep [s] = s
intersperse sep (s1::s2::ss) = s1 ++ sep ++ intersperse sep (s2::ss)

mutual
  public export
  Show Expr where
    show (NumLitE n) = show n
    show (StrLitE s) = "\"" ++ readable s ++ "\""
    show (Bin op x y) = unwords [show x, show op, show y]
    show (VarE v) = show v
    show (FunE f args) = show f ++ "(" ++ show args ++ ")"
    show (NegE x) = "-" ++ show x

  public export
  Show Var where
    show (MkVar v []) = show v
    show (MkVar v is) = show v ++ show is

  public export
  Show Stmt where
    show (If c t) = unwords ["IF", show c, "THEN", show t]
    show (Assign v x) = unwords [show v, "=", show x]
    show (Poke addr dat) = unwords ["POKE",  show addr ++ ", " ++ show dat]
    show (Goto n) = unwords ["GOTO", show n]
    show (Gosub n) = unwords ["GOSUB",  show n]
    show Return = "RETURN"
    show (For v from to step) = unwords ["FOR", show v, "=", show from,  "TO", show to] ++ maybe "" (\x => " STEP " ++ show x) step
    show (Read v) = unwords ["READ", show v]
    show (Next v) = unwords ["NEXT", show v]
    show (Data bs) = unwords ["DATA", show bs] -- intersperse ", " (map show bs)
    show (Print xs newline) = "PRINT " ++ show xs ++ if newline then "" else ";"
    show (PrintH fd xs) = unwords ["PRINT#", show fd ++ ",", show xs]
    show Clr = "CLR"
    show Run = "RUN"
    show (Sys n) = unwords ["SYS", show n]
    show (Open fd dev sec fn) = unwords ["OPEN", intersperse "," [show fd, show dev, show sec, show fn]]
    show (InputH fd vs) = unwords ["INPUT#", show fd, show vs]
    show (Close fd) = unwords ["CLOSE", show fd]
    show (OnGoto x ls) = unwords ["ON", show x, "GOTO", show ls]
    show (OnGosub x ls) = unwords ["ON", show x, "GOSUB", show ls]
    show (Get v) = unwords ["GET", show v]
    show End = "END"
    show Rem = "REM"
