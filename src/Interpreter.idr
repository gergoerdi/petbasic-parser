module Interpreter

import Syntax
import Data.SortedMap
import Data.List1
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont

data V = MkV Var0 (List Int16)

data Value
    = BoolVal Bool
    | NumVal Double
    | StrVal String

mutual
  record S (r : Type) where
    constructor MkS
    lineNum : Maybe LineNum
    vars : SortedMap V Value
    returnConts : List (BASIC r ())
    nextConts : List (BASIC r ())
    actions : List String

  record R (r : Type) where
    constructor MkR
    lineMap : SortedMap LineNum (List1 Stmt, Maybe LineNum)
    abortLineCont : BASIC r ()

  BASIC : Type -> Type -> Type
  BASIC r = ReaderT (R r) (StateT (S r) (ContT r IO))

modify : (S r -> S r) -> BASIC r ()
modify = Control.Monad.State.modify

isTrue : Value -> Bool
isTrue (BoolVal b) = b
isTrue (NumVal x) = x /= 0
isTrue (StrVal s) = s /= ""

toFloat : Value -> Double
toFloat (NumVal x) = x
toFloat (BoolVal b) = if b then 1 else 0
toFloat (StrVal s) = 0/0

getVar : V -> BASIC r Value
getVar var@(MkV var0 idx) = do
    value <- gets $ SortedMap.lookup var . vars
    pure $ case (value, var0) of
        (Just value, _) => value
        (Nothing, IntVar{}) => NumVal 0
        (Nothing, StrVar{}) => StrVal ""
        (Nothing, RealVar{}) => NumVal 0

setVar : V -> Value -> BASIC r ()
setVar var value = modify $ record { vars $= SortedMap.insert var value }

goto : LineNum -> BASIC r ()
goto lineNum = do
  modify $ record { lineNum = Just lineNum }
  ?execLine

returnSub : BASIC r ()
returnSub = do
    (k :: ks) <- gets returnConts
      | [] => assert_total $ idris_crash "gosub stack underflow"
    modify $ record { returnConts = ks }
    k

abortLine : BASIC r ()
abortLine = join $ asks abortLineCont
