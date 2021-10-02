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
data Var0
  = RealVar Id
  | IntVar Id
  | StrVar Id

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

mutual
  public export
  data Var
    = MkVar Var0 (List Expr)

  public export
  data Expr
    = VarE Var
    | StrLitE (List Bits8)
    | NumLitE Number
    -- | EqE Expr Expr
    -- | NEqE Expr Expr
    -- | LTE Expr Expr
    -- | LEE Expr Expr
    -- | GTE Expr Expr
    -- | GEE Expr Expr
    | PlusE Expr Expr
    | MinusE Expr Expr
    | MulE Expr Expr
    | FunE Fun (List1 Expr)
    | AndE Expr Expr
    | OrE Expr Expr
    | NegE Expr

public export
data Stmt
  -- = If Expr Stmt
  = Assign Var Expr
  | Goto LineNum
  | Gosub LineNum
  | Return
  | Poke Expr Expr
  | For Var0 Expr Expr (Maybe Number)
  | Read Var
  | Next Var0
  | Data (List1 Number)
  | Print (List Expr) Bool
  | PrintH Expr (List Expr)
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
readable = pack . map (chr . cast) . toList

public export
Show Id where
  show (MkId nm) = readable nm

public export
Show Var0 where
  show (RealVar nm) = show nm
  show (IntVar nm) = show nm
  show (StrVar nm) = show nm

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

intersperse : String -> List String -> String
intersperse sep [] = ""
intersperse sep [s] = s
intersperse sep (s1::s2::ss) = s1 ++ sep ++ intersperse sep (s2::ss)

mutual
  public export
  Show Expr where
    show (NumLitE n) = show n
    show (StrLitE s) = "\"" ++ readable s ++ "\""
    show (PlusE x y) = show x ++ " + " ++ show y
    show (MinusE x y) = show x ++ " - " ++ show y
    show (MulE x y) = show x ++ " * " ++ show y
    show (VarE v) = show v
    show (FunE f args) = show f ++ "(" ++ show args ++ ")"
    show (AndE x y) = show x ++ " AND " ++ show y
    show (OrE x y) = show x ++ " OR " ++ show y
    show (NegE x) = "-" ++ show x

  public export
  Show Var where
    show (MkVar v []) = show v
    show (MkVar v is) = show v ++ show is

  public export
  Show Stmt where
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
