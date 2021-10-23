import Syntax
import Binary
import Interpreter

import Data.List1
import Control.Monad.Reader
import Control.Monad.State

import JS
import Web.Fetch
import Web.Dom
import Web.Html
import Web.Raw.UIEvents
import Web.Raw.Fetch

%hide Array.fromString
%hide Web.Dom.Alias.Output

%foreign "browser:lambda:(_a, x) => ((console.log(x),x))"
traceConsoleId : a -> a

%foreign "browser:lambda:(_a, _b, x, y) => ((console.log(x),y))"
traceConsole : a -> b -> b

loadGame : HasIO io => UInt8Array -> io (List (LineNum, List1 Stmt))
loadGame buf = liftIO $ evalStateT (the Bits32 0) $ runReaderT buf $ loadList loadLine
  where
    loadLine : Get (LineNum, List1 Stmt)
    loadLine = (,) <$> load <*> loadList1 load

record UI where
  constructor MkUI
  setPic : String -> JSIO ()
  setText : String -> JSIO ()
  setPrompt : String -> JSIO ()
  setActions : List String -> JSIO ()

elementList : HTMLCollection -> JSIO (List Element)
elementList coll = do
  n <- HTMLCollection.length coll
  let loop : Bits32 -> JSIO (List Element)
      loop i = do
        if i < n
          then do
            mx <- HTMLCollection.item coll i
            case mx of
              Nothing => pure []
              Just x => (x ::) <$> loop (i + 1)
          else pure []
  loop 0

zipFrom : Num a => a -> (1 xs : List b) -> List (a, b)
zipFrom i [] = []
zipFrom i (x :: xs) = (i, x) :: zipFrom (i + 1) xs

initUI : JSIO UI
initUI = do
  img <- createElement Ime

  _ <- appendChild !body img

  compass <- newElement Div [id =. "compass"]
  for_ (zipFrom 1 [("n", "É"), ("w", "NY"), ("e","K"), ("s", "D")]) $ \(i, (tag, label)) => do
    span <- newElement Span [id =. ("compass-" <+> tag)]
    a <- newElement A  [href =. "", textContent =. label]
    _ <- appendChild span a
    _ <- appendChild compass span
    pure ()
  _ <- appendChild !body compass

  text <- createElement Pre
  _ <- appendChild !body text

  prompt <- newElement P [id =. "prompt"]
  _ <- appendChild !body prompt

  actions <- createElement Ul
  _ <- appendChild !body actions

  pure $ MkUI
    { setPic = \pic =>  src img .= "assets/pic/" <+> pic <+> ".png"
    , setText = \s => textContent text .= s
    , setPrompt = \s => textContent prompt .= s
    , setActions = \ss => do
        oldActions <- elementList =<< children actions
        traverse_ (removeChild actions) oldActions
        for_ ss $ \action => do
          a <- newElement A [textContent =. action, href =. ""]
          li <- createElement Li
          ignore $ appendChild li a
          ignore $ appendChild actions li
    }

processOutput : UI -> Output -> JSIO Bool
processOutput ui out = case out of
  ChangeRoom pic txt => do
    setPic ui pic
    setText ui txt -- TODO: load text
    pure True
  WaitInput actions => do
    setActions ui actions
    pure False
  Message s => do
    setPrompt ui s
    pure True
  EndGame => do
    pure False -- TODO

partial step : UI -> R -> S -> JSIO S
step ui r s = do
  let loop : S -> JSIO S
      loop s = do
        let (s', out) = runBASIC r s execLine
        continue <- processOutput ui out
        if continue then loop s' else pure s'
  loop s

partial main : IO ()
main = runJS $ do
  ui <- initUI

  p <- fetch "assets/pokol.ppb"
  p <- p `then_` arrayBuffer
  _ <- p `then_` \buf => do
    buf8 <- pure $ the UInt8Array $ cast buf
    printLn "Loaded"
    lines <- loadGame buf8
    printLn "Parsed"
    let (r, s) = startBASIC lines
    setPrompt ui "MIT TESZEL?"
    s' <- step ui r s
    pure $ ready ()

  pure ()
