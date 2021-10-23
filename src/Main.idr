import Syntax
import Binary
import Interpreter
import Text

import Data.List
import Data.List1
import Data.String
import Control.Monad.Reader
import Control.Monad.State

import JS
import Web.Fetch
import Web.Dom
import Web.Html
import Web.Raw.UIEvents
import Web.Raw.Fetch

import Data.IORef

%hide Array.fromString
%hide Web.Dom.Alias.Output
%hide Types.InputEvent

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

data InputEvent = Move Number | Action Number
Show InputEvent where
  show (Move n) = "Move " <+> show n
  show (Action n) = "Action " <+> show n

record App (m : Type -> Type) (input : Type) (output : Type) where
  constructor MkApp
  view : (input -> m ()) -> m (output -> m ())
  model : input -> m output
  initial : output

runApp : HasIO m => App m input output -> m ()
runApp app = do
  box <- newIORef (\i => pure ())
  refresh <- app.view (\i => readIORef box >>= ($ i))
  writeIORef box $ \i => app.model i >>= refresh
  refresh app.initial

data OutputEvent
  = ChangePic String
  | ChangeText String
  | ChangeActions (List String)
  | ChangePrompt String

step : R -> BASIC () -> S -> (S, List Output)
step r act s =
  let (s', out) = runBASIC r s act
      continue = case out of
        WaitInput{} => False
        EndGame{} => False
        _ => True
  in if continue then let (s'', outs) = step r execLine s' in (s'', out::outs) else (s', [out])

app : List (LineNum, List1 Stmt) -> JSIO (App JSIO InputEvent (List OutputEvent))
app lines = do
  let (r, s) = startBASIC lines
  ref <- newIORef s
  let run : BASIC () -> JSIO (List Output)
      run act = do
        s <- readIORef ref
        let (s', out) = step r act s
        writeIORef ref s'
        pure out
  let fromOut : Output -> JSIO (List OutputEvent)
      fromOut = \out => case out of
        ChangeRoom pic txt => do
          -- p <- fetch $ "assets/text/" <+> txt
          -- p <- p `then_` arrayBuffer
          -- _ <- p `then_` \buf => do
          --   buf8 <- pure $ the UInt8Array $ cast buf
          --   s <- textFromBuf buf8
          --   setText ui s -- TODO: load text
          --   pure $ ready ()
          pure [ ChangePic pic
               , ChangeText txt
               , ChangePrompt "MIT TESZEL?"
               ]
        WaitInput actions => do
          pure
            [ ChangeActions actions
            ]
        Message s => do
          pure [ChangePrompt s] -- TODO: wait for click?
        EndGame => do
          pure []
  initial <- map concat $ traverse fromOut =<< run execLine
  pure $ MkApp
    { view = \sink => do
        img <- createElement Ime

        _ <- appendChild !body img

        compass <- newElement Div [id =. "compass"]
        for_ (zipFrom 1 [("n", "É"), ("w", "NY"), ("e","K"), ("s", "D")]) $ \(i, (tag, label)) => do
          span <- newElement Span [id =. ("compass-" <+> tag)]
          a <- newElement A  [href =. "#", textContent =. label]
          onclick a ?> sink $ Move i
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

        pure $ traverse_ $ \out => case (traceConsoleId $ the OutputEvent out) of
          ChangePic pic => src img .= "assets/pic/" <+> pic <+> ".png"
          ChangeText s => textContent text .= s
          ChangePrompt s => textContent prompt .= s
          ChangeActions ss => do
            oldActions <- elementList =<< children actions
            traverse_ (removeChild actions) oldActions
            for_ (zipFrom 1 ss) $ \(i, action) => do
              a <- newElement A [textContent =. action, href =. "#"]
              onclick a ?> sink $ Action i
              li <- createElement Li
              ignore $ appendChild li a
              ignore $ appendChild actions li
    , model = \input => do
        outs <- run $ case input of
          Move n => playerMove n
          Action n => playerAction n
        map concat $ traverse fromOut outs
    , initial = initial
    }

textFromBuf : UInt8Array -> JSIO String
textFromBuf buf = unlines . filter (not . null) . lines . pack . map readable <$> go 2
  where
    go : Bits32 -> JSIO (List Bits8)
    go i = do
      mx <- readIO buf i
      case mx of
        Nothing => pure []
        Just x => (x ::) <$> go (i + 1)

-- processOutput : UI -> Output -> JSIO Bool
-- processOutput ui out = case out of
--   ChangeRoom pic txt => do
--     setPic ui pic
--     p <- fetch $ "assets/text/" <+> txt
--     p <- p `then_` arrayBuffer
--     _ <- p `then_` \buf => do
--       buf8 <- pure $ the UInt8Array $ cast buf
--       s <- textFromBuf buf8
--       setText ui s -- TODO: load text
--       pure $ ready ()
--     pure True
--   WaitInput actions => do
--     setPrompt ui "MIT TESZEL?"
--     setActions ui actions
--     pure False
--   Message s => do
--     setPrompt ui s
--     pure True -- TODO: wait for click?
--   EndGame => do
--     pure False -- TODO

main : IO ()
main = runJS $ do
  p <- fetch "assets/pokol.ppb"
  p <- p `then_` arrayBuffer
  _ <- p `then_` \buf => do
    buf8 <- pure $ the UInt8Array $ cast buf
    putStrLn "Loaded"
    lines <- loadGame buf8
    putStrLn "Parsed"
    app lines >>= runApp
    -- let (r, s) = startBASIC lines
    -- ref <- newIORef s
    -- ui <- initUI $ \ev => printLn ev
    -- let run = \act => readIORef ref >>= step ui r act >>= writeIORef ref
    -- run execLine
    pure $ ready ()

  pure ()
