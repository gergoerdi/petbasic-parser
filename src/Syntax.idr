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

mutual
  public export
  data Var0
    = RealVar Id
    | IntVar Id
    | StrVar Id

  public export
  data Var
    = MkVar Var0 (List Expr)

  public export
  data Expr
    = VarE Var
    | NumLitE Number
    | PlusE Expr Expr

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
  -- | Print [Expr] Bool
  -- | PrintH Expr [Expr]
  -- | Clr
  -- | Run
  -- | Sys Int16
  -- | Open Expr Expr Expr Expr
  -- | InputH Expr [Var]
  -- | Close Expr
  -- | OnGoto Expr [LineNum]
  -- | OnGosub Expr [LineNum]
  -- | Get Var
  -- | End
  -- | Rem


public export
Show Id where
  show (MkId nm) = pack . toList . map (chr . cast) $ nm

mutual
  public export
  Show Expr where
    show (NumLitE n) = show n
    show (PlusE x y) = show x ++ " + " ++ show y
    show (VarE v) = show v

  public export
  Show Var0 where
    show (RealVar nm) = show nm
    show (IntVar nm) = show nm
    show (StrVar nm) = show nm

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
