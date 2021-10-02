module Syntax

import Data.List1

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
    = NumLitE Number

public export
data Stmt
  = Goto LineNum
  | Poke Expr Expr
  | For Var0 Expr Expr (Maybe Number)
  | Read Var
  | Next Var0

public export
Show Id where
  show (MkId nm) = pack . toList . map (chr . cast) $ nm

mutual
  public export
  Show Expr where
    show (NumLitE n) = show n

  public export
  Show Var0 where
    show (RealVar nm) = show nm
    show (IntVar nm) = show nm
    show (StrVar nm) = show nm

  public export
  Show Var where
    show (MkVar v is) = show v ++ show is

  public export
  Show Stmt where
    show (Poke addr dat) = "POKE " ++ show addr ++ ", " ++ show dat
    show (Goto n) = "GOTO " ++ show n
    show (For v from to step) = "FOR " ++ show v ++ " = " ++ show from ++ " TO " ++ show to ++ maybe "" (\x => "STEP " ++ show x) step
    show (Read v) = "READ " ++ show v
    show (Next v) = "NEXT " ++ show v
