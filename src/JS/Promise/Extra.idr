module JS.Promise.Extra

import JS
import public Web.Internal.Types

%foreign "javascript:lambda:(_a, _b, p, k) => p.then(x => k(x)())"
prim__then : {0 a : Type} -> {0 b : Type} -> Promise a -> (a -> PrimIO (Union2 b (Promise b))) -> PrimIO (Promise b)

raw__then : Promise a -> (a -> JSIO (NS I [b, Promise b])) -> JSIO (Promise b)
raw__then p k = primIO $ prim__then p $ \x =>
  toPrim $ eitherT (\err => assert_total $ idris_crash $ dispErr err) (pure . toUnion2) $ k x

infixl 10 `then_`

export
then_ : Promise a -> (a -> JSIO (Promise b)) -> JSIO (Promise b)
then_ p k = raw__then p $ \x => inj <$> k x
  where
    inj : Promise b -> NS I [b, Promise b]
    inj x = inject x

%foreign "javascript:lambda:(_a, x) => new Promise((resolve, reject) => resolve(x))"
raw__ready : {0 a : Type} -> a -> Promise a

export
ready : a -> Promise a
ready = raw__ready

export sequenceP : List (Promise a) -> JSIO (Promise (List a))
sequenceP [] = pure $ ready []
sequenceP (p :: ps) = p `then_` \x => do
  k <- sequenceP ps
  k `then_` \xs => pure $ ready $ x :: xs

export concatP : List (Promise (List a)) -> JSIO (Promise (List a))
concatP ps = do
  p <- sequenceP ps
  p `then_` \xss => pure $ ready $ concat xss
