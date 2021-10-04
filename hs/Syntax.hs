module Syntax where

import Data.Word

type Bits8 = Word8

type Bits16 = Word16
type LineNum = Bits16

type Number = Double
type List = ([])
type List1 = List

data Id = MkId (List1 Bits8)
  deriving (Show)

data Var0
  = RealVar Id
  | IntVar Id
  | StrVar Id
  deriving (Show)

data Fun
  = Peek
  | IntFun
  | Rnd
  | LeftStr
  | Chr
  | Val
  | Asc
  | Tab
  deriving (Show)

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
  deriving (Show)

data Var
  = MkVar Var0 (List Expr)
  deriving (Show)

data Expr
  = VarE Var
  | StrLitE (List Bits8)
  | NumLitE Number
  | Bin BinOp Expr Expr
  | FunE Fun (List1 Expr)
  | NegE Expr
  deriving (Show)

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
  deriving (Show)
