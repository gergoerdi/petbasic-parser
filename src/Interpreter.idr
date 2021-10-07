module Interpreter

import Syntax
import Data.SortedMap
import Data.List1
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont
import Data.String

data V = MkV Var0 (List Int16)

implementation Eq V where
    MkV v is == MkV v' is' = v == v' && is == is'

implementation Ord V where
    compare (MkV v is) (MkV v' is') = compare v v' <+> compare is is'

data Value
    = BoolVal Bool
    | NumVal Double
    | StrVal String

implementation Show Value where
  show (BoolVal b) = show b
  show (NumVal d) = show d
  show (StrVal s) = show s

implementation Eq Value where
  BoolVal b == BoolVal b' = b == b'
  NumVal n == NumVal n' = n == n'
  StrVal s == StrVal s' = s == s'
  _ == _ = False

mutual
  public export
  record S (r : Type) where
    constructor MkS
    lineNum : Maybe LineNum
    vars : SortedMap V Value
    returnConts : List (BASIC r ())
    nextConts : List (BASIC r ())
    actions : List String

  public export
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

plus : Value -> Value -> Value
plus (NumVal x) (NumVal y) = NumVal $ x + y
plus (StrVal s) (NumVal y) = StrVal $ s ++ show y
plus (StrVal s) (StrVal t) = StrVal $ s ++ t
plus _ _ = StrVal "#ERR"

getVar : V -> BASIC r Value
getVar var@(MkV var0 idx) = do
    value <- gets $ SortedMap.lookup var . vars
    pure $ case (value, var0) of
        (Just value, _) => value
        (Nothing, IntVar{}) => NumVal 0
        (Nothing, StrVar{}) => StrVal ""
        (Nothing, RealVar{}) => NumVal 0

mutual
  var : Var -> BASIC r V
  var (MkVar v0 is) = MkV v0 <$> traverse (map (cast . floor . toFloat) . eval) is

  partial eval : Expr -> BASIC r Value
  eval (VarE v) = getVar =<< var v
  eval (NumLitE n) = pure $ NumVal n
  eval (StrLitE n) = pure $ StrVal $ pack . map (chr . cast) $ n
  eval (Bin Eq x y) = BoolVal <$> ((==) <$> eval x <*> eval y)
  eval (Bin Plus x y) = plus <$> eval x <*> eval y
  eval (Bin And x y) = do
    v1 <- isTrue <$> eval x
    v2 <- isTrue <$> eval y
    pure $ BoolVal $ v1 && v2
  eval (Bin Or x y) = do
    v1 <- isTrue <$> eval x
    v2 <- isTrue <$> eval y
    pure $ BoolVal $ v1 || v2
  eval (Bin LT v1 v2) = BoolVal <$> do
    NumVal x <- eval v1
    NumVal y <- eval v2
    pure $ x < y
  eval (Bin GE v1 v2) = BoolVal <$> do
    NumVal x <- eval v1
    NumVal y <- eval v2
    pure $ x > y
  eval e = idris_crash $ show e

setVar : V -> Value -> BASIC r ()
setVar var value = modify $ record { vars $= SortedMap.insert var value }

export
partial execLine : BASIC r ()

goto : LineNum -> BASIC r ()
goto lineNum = do
  modify $ record { lineNum = Just lineNum }
  execLine

returnSub : BASIC r ()
returnSub = do
    (k :: ks) <- gets returnConts
      | [] => assert_total $ idris_crash "gosub stack underflow"
    modify $ record { returnConts = ks }
    k

partial unsafeTail : List a -> List a
unsafeTail (x::xs) = xs

abortLine : BASIC r ()
abortLine = join $ asks abortLineCont

sanitizeLine : String -> String
sanitizeLine = id

partial exec : Stmt -> BASIC r ()
exec (If cond thn) = do
    b <- isTrue <$> eval cond
    unless b abortLine
    exec thn
exec (Assign v e) = do
    v <- var v
    val <- eval e
    setVar v val
exec (Poke addr e) = do
    addr <- eval addr
    val <- eval e
    liftIO $ printLn ("Poke", addr, val)
exec (Goto line) = goto line
exec (Gosub line) = callCC $ \k => do
    modify $ record { returnConts $= (k () ::) }
    goto line
exec Return = returnSub
exec (Print ss newLine) = do
    lineNum <- gets lineNum
    vals <- traverse eval ss
    let str = concat $ map (\ (StrVal s) => s) vals
    case str of
    --     (c:str) | ord c == 158 => liftIO $ putStr $ "MSG: " <> sanitizeLine str
        _ =>  modify $ record { actions $= (++ [sanitizeLine str]) }
    when newLine $ liftIO $ putStrLn ""
exec (For v0 from to mstep) = do
    let v = MkV v0 []
    setVar v =<< eval from
    let next, loop : BASIC r ()
        next = do
            to <- eval to
            current <- getVar v
            let new = current `plus` NumVal step
            setVar v new
            if keepGoing new to then loop else modify $ record { nextConts $= unsafeTail }
        loop = modify $ record { nextConts $= (next::) }
    loop
  where
    step : Number
    step = fromMaybe 1 mstep

    decreasing : Bool
    decreasing = step < 0

    keepGoing : Value -> Value -> Bool
    keepGoing (NumVal x) (NumVal y) = if decreasing then x >= y else x <= y
exec stmt = do
    lineNum <- gets lineNum
    assert_total $ idris_crash $ show (lineNum, stmt)

execLine = do
    -- let input : BASIC r ()
    let input : BASIC r ()
        input = do
            s <- the (S r) <$> get
            traverse_ putStrLn $ actions s

    s <- the (S r) <$> get
    r <- the (R r) <$> ask
    for_ (lineNum s) $ \lineNum => do
        -- liftIO $ print lineNum
        case lineNum of
            _ => do
                let (line, nextLine) = fromMaybe (idris_crash $ unwords ["Missing line", show lineNum]) $ SortedMap.lookup lineNum (lineMap r)
                callCC $ \k => local (record { abortLineCont = k () }) $ traverse_ exec line
                modify $ record { lineNum = nextLine }
                execLine

zipWithNext : (a -> Maybe a -> b) -> List a -> List b
zipWithNext f [] = []
zipWithNext f (x :: []) = [f x Nothing]
zipWithNext f (x :: xs@(x' :: _)) = f x (Just x') :: zipWithNext f xs

export
runBASIC : List (LineNum, List1 Stmt) -> BASIC a a -> IO a
runBASIC lines act = do
    let s = MkS
            { lineNum = Just 1805
            , vars = SortedMap.empty
            , returnConts = []
            , nextConts = []
            , actions = []
            }

    let nextLines = zipWithNext (\ (lineNum, line), nextLine => (lineNum, (line, fst <$> nextLine))) lines
    let lineMap = SortedMap.fromList nextLines

    let r0 = MkR
            { lineMap = lineMap
            , abortLineCont = pure ()
            }

    runContT (evalStateT s $ runReaderT r0 $ act) pure
