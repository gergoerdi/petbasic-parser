module Pokol.UI

import PETBASIC.Syntax
import PETBASIC.Binary
import Pokol.Interpreter
import Pokol.Text
import Pokol.Utils

import Data.Binary
import JS.App
import JS.Promise.Extra

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
import Web.Raw.Css

import Data.IORef

%hide Web.Dom.Alias.Output
%hide Types.InputEvent

%foreign "browser:lambda:(_a, x) => x.offsetWidth"
export prim__offsetWidth : a -> PrimIO Int

textFromBuf : Nat -> UInt8Array -> JSIO String
textFromBuf n buf = unlines . take n . filter (not . null) . lines . pack . map readable <$> go 2
  where
    go : Bits32 -> JSIO (List Bits8)
    go i = do
      mx <- readIO buf i
      case mx of
        Nothing => pure []
        Just x => (x ::) <$> go (i + 1)

data InputEvent = Move Number | Action Number
Show InputEvent where
  show (Move n) = "Move " <+> show n
  show (Action n) = "Action " <+> show n

data OutputEvent
  = ChangePic Nat
  | ChangeText String
  | ChangeActions (List String)
  | ChangePrompt String

fromOutput : Output -> JSIO (Promise (List OutputEvent))
fromOutput out = case out of
   ChangeRoom pic txt n => do
     p <- fetch $ "assets/text/" <+> txt
     p <- p `then_` arrayBuffer
     p `then_` \buf => do
       buf8 <- pure $ the UInt8Array $ cast buf
       s <- textFromBuf n buf8
       pure $ ready $
         [ ChangePic pic
         , ChangeText s
         , ChangePrompt "MIT TESZEL?"
         ]
   WaitInput actions => do
     pure $ ready [ChangeActions actions]
   Message s => do
     pure $ ready [ChangePrompt s] -- TODO: wait for click?
   EndGame => do
     pure $ ready [] -- TODO: wait for click, then end game

step : R -> BASIC () -> S -> (S, List Output)
step r act s =
  let (s', out) = runBASIC r s act
      continue = case out of
        WaitInput{} => False
        EndGame{} => False
        _ => True
  in if continue then let (s'', outs) = step r execLine s' in (s'', out::outs) else (s', [out])

export
app : List (LineNum, List1 Stmt) -> JSIO (App JSIO InputEvent (Promise (List OutputEvent)))
app lines = do
  let (r, s) = startBASIC lines
  ref <- newIORef s
  let run : BASIC () -> JSIO (List Output)
      run act = do
        s <- readIORef ref
        let (s', out) = step r act s
        writeIORef ref s'
        pure out
  initial <- concatP =<< (traverse fromOutput =<< run execLine)
  pure $ MkApp
    { view = \sink => do
        Just pic <- (castTo HTMLImageElement =<<) <$> getElementById !document "pic"
          | _ => assert_total $ idris_crash "HTML mismatch: pic"

        for_ (zipFrom 1 ["n", "w", "e", "s"]) $ \(i, tag) => do
          Just span <- (castTo HTMLSpanElement =<<) <$> getElementById !document ("compass-" <+> tag)
            | _ => assert_total $ idris_crash "HTML mismatch: compass"
          [Just a] <- map (castTo HTMLAnchorElement) <$> (elementList =<< getElementsByTagName span "a")
            | _ => assert_total $ idris_crash "HTML mismatch: compass"
          onclick a ?> sink $ Move i

        [Just text] <- map (castTo HTMLPreElement) <$> (elementList =<< getElementsByTagName !document "pre")
          | _ => assert_total $ idris_crash "HTML mismatch: text"

        Just prompt <- (castTo HTMLParagraphElement =<<) <$> getElementById !document "prompt"
          | _ => assert_total $ idris_crash "HTML mismatch: prompt"

        Just checkbox <- (castTo HTMLInputElement =<<) <$> getElementById !document "tab-inventory"
          | _ => assert_total $ idris_crash "HTML mismatch: tab-inventory"
        onkeydown !document !> \ev => when (not !(repeat ev) && !(key ev) == "t") $ checked checkbox %= not

        pure $ \p => ignore $ (p `then_`) $ \outs => (ready () <$) $ for_ outs $ \out => case out of
          ChangePic idx => do
            sty <- style pic
            current <- trim <$> CSSStyleDeclaration.getPropertyValue !(getComputedStyle' !window pic) "--pic-idx"
            unless (current == show idx) $ do
              CSSStyleDeclaration.setProperty' sty "--pic-idx-prev" current
              CSSStyleDeclaration.setProperty' sty "--pic-idx" $ show idx
              _ <- toggle !(classList pic) "pic-trigger" (Def True)
              _ <- primIO $ prim__offsetWidth pic
              _ <- toggle !(classList pic) "pic-trigger" (Def False)
              checked checkbox .= False
          ChangeText s => textContent text .= s
          ChangePrompt s => textContent prompt .= s
          ChangeActions ss => do
            Just oldActions <- (castTo HTMLUListElement =<<) <$> getElementById !document "actions"
              | _ => assert_total $ idris_crash "HTML mismatch: actions"
            newActions <- newElement Ul [id =. "actions"]
            for_ (zipFrom 1 ss) $ \(i, action) => unless (null action) $ do
              a <- newElement A [textContent =. action, href =. "#"]
              onclick a ?> sink $ Action i
              li <- createElement Li
              ignore $ appendChild li a
              ignore $ appendChild newActions li
              oldActions `replaceWith` [inject $ newActions :> Node]
    , model = \input => do
        outs <- run $ case input of
          Move n => playerMove n
          Action n => playerAction n
        concatP =<< traverse fromOutput outs
    , initial = initial
    }
