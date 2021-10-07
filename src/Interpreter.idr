module Interpreter

import Syntax
import Data.SortedMap
import Data.List
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

total index1OrLast : Nat -> List1 a -> a
index1OrLast n (x ::: xs) = case n of
  Z => x
  S n => go n x xs
  where
    go : Nat -> a -> List a -> a
    go _     x []        = x
    go Z     _ (x::xs)   = x
    go (S n) _ (x :: xs) = go n x xs

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
    case unpack str of
        (c::str') =>
          if ord c == 158 then do liftIO $ putStr $ "MSG: " <+> sanitizeLine (pack str')
          else modify $ record { actions $= (<+> [sanitizeLine str]) }
        _ =>  modify $ record { actions $= (<+> [sanitizeLine str]) }
    when newLine $ liftIO $ putStrLn ""
exec (OnGoto e lines) = do
    (NumVal val) <- eval e
    let line = index1OrLast (cast $ val - 1) lines
    goto line
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

execLine = do
    let input : BASIC r ()
        input = do
            s <- the (S r) <$> get
            printActions $ actions s
            putStr "> "

            let move : Number -> BASIC r ()
                move = \dir => do
                    liftIO $ putStrLn $ unwords ["MOVE ", show dir]
                    setVar (MkV (RealVar . MkId $ map cast $ 'M' ::: ['H']) []) $ NumVal 2
                    setVar (MkV (RealVar . MkId $ map cast $ 'Z' ::: []) []) $ NumVal dir
                action : Number -> BASIC r ()
                action = \sel => do
                    liftIO $ putStrLn $ unwords ["DO ", show sel]
                    setVar (MkV (RealVar . MkId $ map cast $ 'M' ::: ['H']) []) $ NumVal 1
                    setVar (MkV (RealVar . MkId $ map cast $ 'M' ::: ['A']) []) $ NumVal sel
            s <- toLower <$> liftIO getLine
            case words s of
                ["do", n] => action $ cast n
                ["go", n] => move $ cast n
                _ => input
            pure ()

    s <- the (S r) <$> get
    r <- the (R r) <$> ask
    for_ (lineNum s) $ \lineNum => do
        -- liftIO $ print lineNum
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
                text <- getVar $ MkV (StrVar . MkId $ map cast $ 'T' ::: []) []
                ab <- getVar $ MkV (RealVar . MkId $ map cast $ 'A' ::: ['B']) []
                fb <- getVar $ MkV (RealVar . MkId $ map cast $ 'F' ::: ['B']) []
                let pict = pack [chr (cast . floor $ v) | NumVal v <- [ab, fb]]
                liftIO $ putStrLn $ unwords ["PICTURE", pict]
                -- liftIO $ do
                --     let StrVal s = text
                --         textFile = "disk/" <+> map toLower s
                --     textLines <- loadText textFile
                --     putStrLn $ unwords textLines
                returnSub
            10200 => do
                modify $ record { actions = [] }
                returnSub
            10394 => do
                input
                returnSub
            10455 => do
                input
                -- error "done"
                returnSub
            10640 => do
                -- error "done"
                returnSub
            10510 => do
                goto 10600
            10600 => do
                returnSub
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
