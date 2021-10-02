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
    -- | StrLitE (List Bits8)
    | NumLitE Number
    -- | EqE Expr Expr
    -- | NEqE Expr Expr
    -- | LTE Expr Expr
    -- | LEE Expr Expr
    -- | GTE Expr Expr
    -- | GEE Expr Expr
    | PlusE Expr Expr
    -- | MinusE Expr Expr
    -- | MulE Expr Expr
    | FunE Fun (List1 Expr)
    | AndE Expr Expr
    | OrE Expr Expr
    -- | NegE Expr

public export
data Stmt
  -- = If Expr Stmt
  -- | Assign Var Expr
  = Goto LineNum
  -- | Gosub LineNum
  -- | Return
  | Poke Expr Expr
  | For Var0 Expr Expr (Maybe Number)
  | Read Var
  | Next Var0
  | Data (List1 Number)
  -- | Print (List Expr) Bool
  -- | PrintH Expr (List Expr)
  -- | Clr
  -- | Run
  -- | Sys Int16
  -- | Open Expr Expr Expr Expr
  -- | InputH Expr (List Var)
  -- | Close Expr
  -- | OnGoto Expr (List1 LineNum)
  -- | OnGosub Expr (List1 LineNum)
  -- | Get Var
  -- | End
  -- | Rem


public export
Show Id where
  show (MkId nm) = pack . toList . map (chr . cast) $ nm

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

mutual
  public export
  Show Expr where
    show (NumLitE n) = show n
    show (PlusE x y) = show x ++ " + " ++ show y
    show (VarE v) = show v
    show (FunE f args) = show f ++ "(" ++ show args ++ ")"
    show (AndE x y) = show x ++ " AND " ++ show y
    show (OrE x y) = show x ++ " OR " ++ show y

  public export
  Show Var where
    show (MkVar v []) = show v
    show (MkVar v is) = show v ++ show is

  public export
  Show Stmt where
    show (Poke addr dat) = "POKE " ++ show addr ++ ", " ++ show dat
    show (Goto n) = "GOTO " ++ show n
    show (For v from to step) = "FOR " ++ show v ++ " = " ++ show from ++ " TO " ++ show to ++ maybe "" (\x => "STEP " ++ show x) step
    show (Read v) = "READ " ++ show v
    show (Next v) = "NEXT " ++ show v
    show (Data bs) = "DATA " ++ show bs -- intersperse ", " (map show bs)
