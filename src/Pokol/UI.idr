module Pokol.UI

import PETBASIC.Syntax
import PETBASIC.Binary
import Pokol.Interpreter
import Pokol.Text
import Pokol.Utils

import Data.Binary
import JS.App

import Data.List
import Data.List1
import Data.String
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity

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

%foreign "browser:lambda:(n,key) => texts[key].slice(0,n).join('\\n')"
prim__lookupText : Int -> String -> String

textFromBuf : Nat -> UInt8Array -> JSIO String
textFromBuf n buf = unlines . take n . filter (not . null) . lines . pack . map readable <$> go 2
  where
    go : Bits32 -> JSIO (List Bits8)
    go i = do
      mx <- readIO buf i
      case mx of
        Nothing => pure []
        Just x => (x ::) <$> go (i + 1)

data InputEvent
 = Unpause
 | Move Number
 | Action Number

Show InputEvent where
  show Unpause = "Unpause"
  show (Move n) = "Move " <+> show n
  show (Action n) = "Action " <+> show n

data OutputEvent
  = ChangePic Nat
  | ChangeText String
  | ChangeActions (List String)
  | ChangePrompt String
  | InventoryItems (List String)
  | Pause Bool
  | ShowInventory

fromOutput : W -> Output -> JSIO (List OutputEvent)
fromOutput w out = case out of
   ChangeRoom pic txt n => do
     pure $
       [ ChangePic pic
       , ChangeText $ prim__lookupText (cast n) txt
       , ChangePrompt "MIT TESZEL?"
       ]
   WaitInput => do
     pure [ChangeActions w.actions]
   Message s => do
     pure [ChangePrompt s]
   EndGame => do
     pure [Pause True] -- TODO: wait for click, then end game
   Pause => do
     pure [Pause True]
   ShowInventory => do
     pure [ShowInventory]

step : MonadBASICIO m => R -> BASIC m () -> S -> m (S, List Output)
step r act s = do
  (s', out) <- runBASIC r s act
  let continue = case out of
        WaitInput{} => False
        EndGame{} => False
        Pause{} => False
        _ => True
  if continue
    then do
      (s'', outs) <- step r execLine s'
      pure (s'', out::outs)
    else do
      pure (s', [out])

export
app : List (LineNum, List1 Stmt) -> JSIO (App JSIO InputEvent (List OutputEvent))
app lines = do
  let (r, s) = startBASIC lines
  ref <- newIORef (s, neutral)
  let run : BASIC (Writer W) () -> JSIO (List OutputEvent)
      run act = do
        (s, w) <- readIORef ref
        ((s, outs), w') <- pure $ runWriter $ step r act s
        traverse_ printLn outs
        w <- pure $ w <+> w'
        writeIORef ref (s, w)
        let inventory = execWriter $ step r (goto 9120 *> execLine) s
        pure $ [Pause False] <+> concat !(traverse (fromOutput w) outs) <+> [InventoryItems inventory]
  initial <- run execLine

  Just pic <- (castTo HTMLImageElement =<<) <$> getElementById !document "pic"
    | _ => assert_total $ idris_crash "HTML mismatch: pic"

  [Just text] <- map (castTo HTMLPreElement) <$> (elementList =<< getElementsByTagName !document "pre")
    | _ => assert_total $ idris_crash "HTML mismatch: text"

  Just prompt <- getElementById !document "prompt"
    | _ => assert_total $ idris_crash "HTML mismatch: prompt"

  Just showInventory <- (castTo HTMLInputElement =<<) <$> getElementById !document "tab-inventory"
    | _ => assert_total $ idris_crash "HTML mismatch: tab-inventory"
  onkeydown !document !> \ev =>
    when (not !(repeat ev) && !(key ev) == "t") $ checked showInventory %= not

  dirs <- for (zipFrom 1 ["n", "w", "e", "s"]) $ \(i, tag) => do
    Just a <- (castTo HTMLAnchorElement =<<) <$> getElementById !document ("compass-" <+> tag)
      | _ => assert_total $ idris_crash "HTML mismatch: compass"
    pure (i, a)

  Just actions <- (castTo HTMLUListElement =<<) <$> getElementById !document "actions"
    | _ => assert_total $ idris_crash $ "HTML mismatch: actions"

  Just pause <- (castTo HTMLAnchorElement =<<) <$> getElementById !document "pause"
    | _ => assert_total $ idris_crash $ "HTML mismatch: pause"

  Just inventory <- getElementById !document "inventory"
    | _ => assert_total $ idris_crash $ "HTML mismatch: inventory"

  pure $ MkApp
    { view = \sink => do
        for_ dirs $ \(i, a) => onclick a ?> sink $ Move i
        onclick pause ?> sink Unpause

        pure $ traverse_ $ \out => case out of
          Pause wait => do
            CSSStyleDeclaration.setProperty' !(style actions) "visibility" (if wait then "hidden" else "visible")
            CSSStyleDeclaration.setProperty' !(style pause) "display" (if wait then "block" else "none")
          ChangePic idx => do
            sty <- style pic
            current <- trim <$> CSSStyleDeclaration.getPropertyValue !(getComputedStyle' !window pic) "--pic-idx"
            unless (current == show idx) $ do
              CSSStyleDeclaration.setProperty' sty "--pic-idx-prev" current
              CSSStyleDeclaration.setProperty' sty "--pic-idx" $ show idx
              _ <- toggle !(classList pic) "pic-trigger" (Def True)
              -- _ <- getComputedStyle' !window pic
              _ <- primIO $ prim__offsetWidth pic
              _ <- toggle !(classList pic) "pic-trigger" (Def False)
              checked showInventory .= False
          ChangeText s => textContent text .= s
          ChangePrompt s => textContent prompt .= s
          ChangeActions ss => do
            items <- for (filter (not . null . snd) (zipFrom 1 ss)) $ \(i, action) => do
              a <- newElement A [textContent =. action, href =. "#"]
              onclick a ?> sink $ Action i
              li <- createElement Li
              ignore $ appendChild li a
              pure $ inject $ li :> Node
            replaceChildren actions items
          InventoryItems ss => do
            items <- for ss $ \s => do
              li <- newElement Li [textContent =. s]
              pure $ inject $ li :> Node
            replaceChildren inventory items
          ShowInventory => do
            checked showInventory .= True

    , model = \input => run $ case input of
        Unpause => execLine
        Move n => playerMove n
        Action n => playerAction n
    , initial = initial
    }
