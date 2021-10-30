module Pokol.Interpreter

import PETBASIC.Syntax
import Pokol.Text

import Data.SortedMap
import Data.List
import Data.List1
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Either
import Data.String

import Debug.Trace

data V = MkV Var0 (List Int16)

implementation Eq V where
    MkV v is == MkV v' is' = v == v' && is == is'

mkV : (Id -> Var0) -> (s : String) -> {auto 0 ok : NonEmpty (unpack s) } -> List Int16 -> V
mkV con nm is = MkV (con . MkId $ map cast $ toList1 (unpack nm)) is

implementation Ord V where
    compare (MkV v is) (MkV v' is') = compare v v' <+> compare is is'

implementation Show V where
    show (MkV v0 is) = show v0 <+> show is

data Value
    = BoolVal Bool
    | NumVal Double
    | StrVal (List Bits8)

implementation Show Value where
  show (BoolVal b) = show b
  show (NumVal d) = show d
  show (StrVal s) = show s

implementation Eq Value where
  BoolVal b == BoolVal b' = b == b'
  NumVal n == NumVal n' = n == n'
  StrVal s == StrVal s' = s == s'
  _ == _ = False

implementation Ord Value where
  compare (BoolVal b) (BoolVal b') = compare b b'
  compare (NumVal n) (NumVal n') = compare n n'
  compare (StrVal s) (StrVal s') = compare s s'
  compare _ _ = LT

Cont : Type
Cont = (LineNum, List Stmt, Maybe LineNum)

LineMap : Type
LineMap = SortedMap LineNum (List1 Stmt, Maybe LineNum)

mutual
  public export
  record S where
    constructor MkS
    vars : SortedMap V Value
    currLine : Cont
    returnConts : List Cont
    nextConts : List Cont

  public export
  record R where
    constructor MkR
    lineMap : LineMap

  public export
  data Output
    = EndGame
    | Message String
    | ChangeRoom Nat String Nat
    | WaitInput

  public export -- XXX how do we export just the Monad &c. implementations?
  BASIC : (Type -> Type) -> Type -> Type
  BASIC m = EitherT Output (ReaderT R (StateT S m))

export
Show Output where
  show EndGame = "EndGame"
  show (Message s) = "Message " <+> s
  show (ChangeRoom pictTag textTag lines) = unwords ["ChangeRoom", show pictTag, show textTag, show lines]
  show WaitInput = "WaitInput"

modify : Monad m => (S -> S) -> BASIC m ()
modify = Control.Monad.State.modify

isTrue : Value -> Bool
isTrue (BoolVal b) = b
isTrue (NumVal x) = x /= 0
isTrue (StrVal s) = not $ null s

toFloat : Value -> Double
toFloat (NumVal x) = x
toFloat (BoolVal b) = if b then 1 else 0
toFloat (StrVal s) = 0/0

plus : Value -> Value -> Value
plus (NumVal x) (NumVal y) = NumVal $ x + y
plus (StrVal s) (NumVal y) = StrVal $ s ++ (map cast . unpack $ show y)
plus (StrVal s) (StrVal t) = StrVal $ s ++ t
plus _ _ = StrVal . map cast . unpack $ "#ERR"

binNum : (Number -> Number -> Number) -> (Value -> Value -> Value)
binNum f (NumVal x) (NumVal y) = NumVal $ f x y
binNum f _          _          = NumVal (0/0)

unNum : (Number -> Number) -> (Value -> Value)
unNum f (NumVal x) = NumVal $ f x
unNum f _          = NumVal (0/0)

getVar : Monad m => V -> BASIC m Value
getVar var@(MkV var0 idx) = do
  value <- gets $ SortedMap.lookup var . vars
  pure $ case (value, var0) of
    (Just value, _) => value
    (Nothing, IntVar{}) => NumVal 0
    (Nothing, StrVar{}) => StrVal empty
    (Nothing, RealVar{}) => NumVal 0

evalBin : BinOp -> Value -> Value -> Value
evalBin Plus = plus
evalBin Minus = binNum (-)
evalBin Mul = binNum (*)
evalBin Eq = \x,y => BoolVal $ x == y
evalBin NEq = \x,y => BoolVal $ x /= y
evalBin LT = \x,y => BoolVal $ x < y
evalBin LE = \x,y => BoolVal $ x <= y
evalBin GT = \x,y => BoolVal $ x > y
evalBin GE = \x,y => BoolVal $ x >= y
evalBin And = \x,y => BoolVal $ isTrue x && isTrue y
evalBin Or = \x,y => BoolVal $ isTrue x || isTrue y

mutual
  var : Monad m => Var -> BASIC m V
  var (MkVar v0 is) = MkV v0 <$> traverse (map (cast . floor . toFloat) . eval) is

  eval : Monad m => Expr -> BASIC m Value
  eval (VarE v) = getVar =<< var v
  eval (NumLitE n) = pure $ NumVal n
  eval (StrLitE bs) = pure $ StrVal bs
  eval (Bin op x y) = evalBin op <$> eval x <*> eval y
  eval (NegE x) = unNum negate <$> eval x
  eval e@(FunE f x) = assert_total $ idris_crash $ show e

setVar : Monad m => V -> Value -> BASIC m ()
setVar var value = modify $ record { vars $= SortedMap.insert var value }

loadLine : LineMap -> LineNum -> Cont
loadLine lineMap lineNum = case lookup lineNum lineMap of
  Nothing => assert_total $ idris_crash $ unwords ["loadLine", show lineNum]
  Just (ss, nextLine) => (lineNum, toList ss, nextLine)

export
goto : Monad m => LineNum -> BASIC m ()
goto lineNum = do
  lineMap <- asks lineMap
  modify $ record { currLine = loadLine lineMap lineNum }

gosub : Monad m => LineNum -> BASIC m ()
gosub lineNum = do
  k <- gets currLine
  modify $ record { returnConts $= (k::) }
  goto lineNum

returnSub : Monad m => BASIC m ()
returnSub = do
  (k :: ks) <- gets returnConts
    | [] => throwE EndGame
  modify $ record { returnConts = ks, currLine = k }

partial unsafeTail : List a -> List a
unsafeTail (x::xs) = xs

gotoNext : Monad m => BASIC m ()
gotoNext = do
  k <- gets currLine
  let (_, _, nextLine) = k
  Just nextLine <- pure nextLine
    | Nothing => throwE EndGame
  goto nextLine

total index1OrLast : Nat -> List1 a -> a
index1OrLast n (x ::: xs) = case n of
  Z => x
  S n => go n x xs
  where
    go : Nat -> a -> List a -> a
    go _     x []        = x
    go Z     _ (x::xs)   = x
    go (S n) _ (x :: xs) = go n x xs

public export
interface Monad m => MonadBASICIO m where
  doPrint : List Bits8 -> BASIC m ()
  doClearActions : BASIC m ()

public export
record W where
  constructor MkW
  actions : List String
  nextActionClears : Bool

public export
Semigroup W where
  MkW as clr <+> MkW as' clr' =
    if clr && not (null as') then MkW as' clr'
      else MkW (as <+> as') (clr || clr')

public export
Monoid W where
  neutral = MkW neutral False

export
Monad m => MonadBASICIO (WriterT W m) where
  doClearActions = tell $ MkW neutral True

  doPrint bs = do
    let action = sanitizeLine bs
        newAction = unless (null . trim $ action) $ tell $ MkW [action] False
    case bs of
      (c::bs') =>
        if c == 158 then do
          doClearActions
          throwE . Message $ sanitizeLine bs'
        else newAction
      _ =>  newAction

export
Monad m => MonadBASICIO (WriterT (List String) m) where
  doClearActions = pure ()
  doPrint bs = tell [sanitizeLine bs]

exec : MonadBASICIO m => Stmt -> BASIC m ()
exec (If cond thn) = do
  b <- isTrue <$> eval cond
  if b then exec thn else gotoNext
exec (Assign v e) = do
  v <- var v
  val <- eval e
  setVar v val
exec (Poke addr e) = do
  addr <- eval addr
  val <- eval e
  pure () -- We should be able to ignore these; the important ones are
          -- at special line locations.
exec (Goto line) = goto line
exec (Gosub line) = gosub line
exec Return = returnSub
exec (Print ss newLine) = do
  -- lineNum <- gets lineNum
  vals <- traverse eval ss
  let strVal : Value -> List Bits8
      strVal = \val => case val of
        StrVal bs => bs
        _        => []
  let bs = concat $ map strVal vals
  doPrint bs
exec (OnGoto e lines) = do
  NumVal val <- eval e
    | e => assert_total $ idris_crash $ unwords ["onGoto", show e]
  let line = index1OrLast (cast $ val - 1) lines
  goto line
exec (OnGosub e lines) = do
  NumVal val <- eval e
    | e => assert_total $ idris_crash $ unwords ["onGosub", show e]
  let line = index1OrLast (cast $ val - 1) lines
  gosub line
-- exec (For v0 from to mstep) = do
--   let v = MkV v0 []
--   setVar v =<< eval from
--   let next, loop : BASIC ()
--       next = do
--           to <- eval to
--           current <- getVar v
--           let new = current `plus` NumVal step
--           setVar v new
--           if keepGoing new to then loop else modify $ record { nextConts $= unsafeTail }
--       -- loop = modify $ record { nextConts $= (next::) }
--   loop
--   where
--     step : Number
--     step = fromMaybe 1 mstep

--     decreasing : Bool
--     decreasing = step < 0

--     keepGoing : Value -> Value -> Bool
--     keepGoing (NumVal x) (NumVal y) = if decreasing then x >= y else x <= y
--     keepGoing _ _ = False
exec stmt = do
  k <- gets currLine
  assert_total $ idris_crash $ show (k, stmt)

numberedFrom : Nat -> List a -> List (Nat, a)
numberedFrom n [] = []
numberedFrom n (x::xs) = (n, x) :: numberedFrom (S n) xs

numbered : List a -> List (Nat, a)
numbered = numberedFrom 0

printActions : (HasIO io) => List String -> io ()
printActions actions = for_ (numberedFrom 1 actions) $ \(i, s) => do
  print i
  putStr " "
  putStrLn s

export
playerMove : Monad m => Number -> BASIC m ()
playerMove dir = do
  setVar (mkV RealVar "MH" []) $ NumVal 2
  setVar (mkV RealVar "Z" []) $ NumVal dir

export
playerAction : Monad m => Number -> BASIC m ()
playerAction i = do
  setVar (mkV RealVar "MH" []) $ NumVal 1
  setVar (mkV RealVar "MA" []) $ NumVal i

playerInput : MonadBASICIO m => BASIC m ()
playerInput = do
  doClearActions
  throwE WaitInput

execStmts : MonadBASICIO m => BASIC m ()
execStmts = do
  k <- gets currLine
  let (lineNum, stmts, nextLine) = k
  (s::stmts) <- pure stmts
    | [] => gotoNext
  modify $ record { currLine = (lineNum, stmts, nextLine) }
  exec s

export
execLine : MonadBASICIO m => BASIC m ()
execLine = do
    s <- get
    let (lineNum, _, _) = currLine s
    do
        -- printLn lineNum
        case lineNum of
            132 => do
                throwE EndGame -- DIE
            3610 => do
                -- liftIO $ putStrLn "PAUSE"
                goto 3620
            4540 => do
                -- TODO: wait for click
                goto 4544
            9015 => do
                -- liftIO $ putStrLn "CLRSCR"
                returnSub
            9600 => do
                -- liftIO $ putStrLn "copy protection"
                returnSub
            9790 => do
                trace "PAUSE" $ pure ()
                -- liftIO $ putStrLn "PAUSE"
                returnSub
            9970 => do
                goto 10020
            10020 => do
                ab <- getVar $ mkV RealVar "AB" []
                fb <- getVar $ mkV RealVar "FB" []
                let numVal : Value -> Nat
                    numVal = \val => case val of
                      NumVal v => cast $ v - 0x30
                      _        => 0
                let pictTag = numVal ab * 10 + numVal fb
                StrVal s <- getVar $ mkV StrVar "T" []
                  | _ => assert_total $ idris_crash "Type error?! In MY BASIC program?!"
                tv <- getVar $ mkV RealVar "TV" []
                let textTag = pack . map (toLower . cast) $ s
                returnSub
                throwE $ ChangeRoom pictTag textTag (if isTrue tv then 6 else 7)
            10200 => do
                doClearActions
                returnSub
            10394 => do
                returnSub
                playerInput
            10455 => do
                -- TODO: good checkpoint for saving/loading
                returnSub
                playerInput
            10640 => do
                returnSub
                playerInput
            10510 => do
                goto 10600
            10600 => do
                -- TODO: good checkpoint for saving/loading
                returnSub
            _ => do
              execStmts

zipWithNext : (a -> Maybe a -> b) -> List a -> List b
zipWithNext f [] = []
zipWithNext f (x :: []) = [f x Nothing]
zipWithNext f (x :: xs@(x' :: _)) = f x (Just x') :: zipWithNext f xs

export
runBASIC : MonadBASICIO m => R -> S -> BASIC m () -> m (S, Output)
runBASIC r s act = do
  (s', res) <- runStateT s . runReaderT r . runEitherT $ act
  case res of
    Left ev => pure (s', ev)
    Right () => runBASIC r s' execLine

export
startBASIC : List (LineNum, List1 Stmt) -> (R, S)
startBASIC lines =
  let nextLines = zipWithNext (\ (lineNum, line), nextLine => (lineNum, (line, fst <$> nextLine))) lines
      lineMap = SortedMap.fromList nextLines

      s0 = MkS
        { currLine = loadLine lineMap 1805
        , vars = empty
        , returnConts = empty
        , nextConts = empty
        }

      r = MkR
         { lineMap = lineMap
         }
  in (r, s0)
