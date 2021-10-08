module Interpreter

import Syntax
import Text

import Data.SortedMap
import Data.List
import Data.List1
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Maybe
import Data.String

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
    actions : List String

  public export
  record R where
    constructor MkR
    lineMap : LineMap

  public export -- XXX how do we export just the Monad &c. implementations?
  BASIC : Type -> Type
  BASIC = MaybeT (ReaderT R (StateT S IO))

modify : (S -> S) -> BASIC ()
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

getVar : V -> BASIC Value
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
evalBin GE = \x,y => BoolVal $ x > y
evalBin GT = \x,y => BoolVal $ x >= y
evalBin And = \x,y => BoolVal $ isTrue x && isTrue y
evalBin Or = \x,y => BoolVal $ isTrue x || isTrue y

mutual
  var : Var -> BASIC V
  var (MkVar v0 is) = MkV v0 <$> traverse (map (cast . floor . toFloat) . eval) is

  eval : Expr -> BASIC Value
  eval (VarE v) = getVar =<< var v
  eval (NumLitE n) = pure $ NumVal n
  eval (StrLitE bs) = pure $ StrVal bs
  eval (Bin op x y) = evalBin op <$> eval x <*> eval y
  eval (NegE x) = unNum negate <$> eval x
  eval e@(FunE f x) = assert_total $ idris_crash $ show e

setVar : V -> Value -> BASIC ()
setVar var value = modify $ record { vars $= SortedMap.insert var value }

loadLine : LineMap -> LineNum -> Cont
loadLine lineMap lineNum = case lookup lineNum lineMap of
  Nothing => assert_total $ idris_crash $ unwords ["loadLine", show lineNum]
  Just (ss, nextLine) => (lineNum, toList ss, nextLine)

goto : LineNum -> BASIC ()
goto lineNum = do
  lineMap <- asks lineMap
  modify $ record { currLine = loadLine lineMap lineNum }

gosub : LineNum -> BASIC ()
gosub lineNum = do
  k <- gets currLine
  modify $ record { returnConts $= (k::) }
  goto lineNum

returnSub : BASIC ()
returnSub = do
  (k :: ks) <- gets returnConts
    | [] => assert_total $ idris_crash "gosub stack underflow"
  modify $ record { returnConts = ks, currLine = k }

partial unsafeTail : List a -> List a
unsafeTail (x::xs) = xs

gotoNext : BASIC ()
gotoNext = do
  k <- gets currLine
  let (_, _, nextLine) = k
  Just nextLine <- pure nextLine
    | Nothing => empty
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

partial exec : Stmt -> BASIC ()
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
  let str = concat $ map (\ (StrVal s) => s) vals
  case str of
      (c::str') =>
        if c == 158 then do putStr $ "MSG: " <+> sanitizeLine str'
        else modify $ record { actions $= (<+> [sanitizeLine str]) }
      _ =>  modify $ record { actions $= (<+> [sanitizeLine str]) }
  when newLine $ liftIO $ putStrLn ""
exec (OnGoto e lines) = do
  (NumVal val) <- eval e
  let line = index1OrLast (cast $ val - 1) lines
  goto line
exec (OnGosub e lines) = do
  (NumVal val) <- eval e
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

playerMove : Number -> BASIC ()
playerMove dir = do
  putStrLn $ unwords ["MOVE ", show dir]
  setVar (mkV RealVar "MH" []) $ NumVal 2
  setVar (mkV RealVar "Z" []) $ NumVal dir

playerAction : Number -> BASIC ()
playerAction i = do
  putStrLn $ unwords ["DO ", show i]
  setVar (mkV RealVar "MH" []) $ NumVal 1
  setVar (mkV RealVar "MA" []) $ NumVal i

playerInput : BASIC ()
playerInput = do
  s <- get
  printActions $ actions s
  putStr "> "

  s <- toLower <$> liftIO getLine
  case words s of
    ["do", n] => playerAction $ cast n
    ["go", n] => playerMove $ cast n
    _ => playerInput

partial execStmts : BASIC ()
execStmts = do
  k <- gets currLine
  let (lineNum, stmts, nextLine) = k
  (s::stmts) <- pure stmts
    | [] => gotoNext
  modify $ record { currLine = (lineNum, stmts, nextLine) }
  exec s

export
partial execLine : BASIC ()
execLine = do
    s <- get
    let (lineNum, _, _) = currLine s
    do
        -- printLn lineNum
        case lineNum of
            132 => do
                -- liftIO $ mapM_ putStrLn actions
                idris_crash "DIE"
            3610 => do
                liftIO $ putStrLn "PAUSE"
                goto 3620
            9015 => do
                liftIO $ putStrLn "CLRSCR"
                returnSub
            9600 => do
                liftIO $ putStrLn "copy protection"
                returnSub
            9790 => do
                liftIO $ putStrLn "PAUSE"
                returnSub
            9970 => do
                goto 10020
            10020 => do
                do
                  ab <- getVar $ mkV RealVar "AB" []
                  fb <- getVar $ mkV RealVar "FB" []
                  let pict = pack [chr (cast . floor $ v) | NumVal v <- [ab, fb]]
                  putStrLn $ unwords ["PICTURE", pict]
                do
                  text <- getVar $ mkV StrVar "T" []
                  let StrVal s = text
                      textFile = "disk/" <+> (pack . map (toLower . cast) $ s)
                  textLines <- loadText textFile
                  -- putStrLn $ unwords textLines
                  traverse_ putStrLn textLines
                returnSub
            10200 => do
                modify $ record { actions = empty }
                returnSub
            10394 => do
                playerInput
                returnSub
            10455 => do
                playerInput
                -- error "done"
                returnSub
            10640 => do
                idris_crash "load/save/move/targyak"
                returnSub
            10510 => do
                goto 10600
            10600 => do
                returnSub
            _ => do
              execStmts

zipWithNext : (a -> Maybe a -> b) -> List a -> List b
zipWithNext f [] = []
zipWithNext f (x :: []) = [f x Nothing]
zipWithNext f (x :: xs@(x' :: _)) = f x (Just x') :: zipWithNext f xs

export
runBASIC : List (LineNum, List1 Stmt) -> BASIC a -> IO (Maybe a)
runBASIC lines act = do
  let nextLines = zipWithNext (\ (lineNum, line), nextLine => (lineNum, (line, fst <$> nextLine))) lines
  let lineMap = SortedMap.fromList nextLines

  let s = MkS
        { currLine = loadLine lineMap 1805
        , vars = empty
        , returnConts = empty
        , nextConts = empty
        , actions = empty
        }

  let r0 = MkR
          { lineMap = lineMap
          }

  evalStateT s . runReaderT r0 . runMaybeT $ act
