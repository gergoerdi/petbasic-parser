module Syntax

public export
LineNum : Type
LineNum = Bits16

public export
data Expr = NumLitE Double

public export
Show Expr where
  show (NumLitE n) = show n

public export
data Stmt = Goto LineNum | Poke Expr Expr

public export
Show Stmt where
  show (Poke addr dat) = "POKE " ++ show addr ++ ", " ++ show dat
  show (Goto n) = "GOTO " ++ show n
