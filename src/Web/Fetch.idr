module Web.Fetch

import JS
import Web.Dom
import Web.Raw.Fetch

%foreign "browser:lambda:r => fetch(r)"
prim__fetch : Union2 String Request -> PrimIO (Promise Response)

raw__fetch : NS I [String, Request] -> JSIO (Promise Response)
raw__fetch = primIO . prim__fetch . toUnion2

export
fetch : {auto prf : Elem a [String, Request]} -> a -> JSIO (Promise Response)
fetch {prf = prf} x = raw__fetch $ inject {prf = prf} x

%foreign "javascript:lambda:(_a, _b, p, k) => p.then(x => k(x)())"
prim__then : {0 a : Type} -> {0 b : Type} -> Promise a -> (a -> PrimIO (Union2 b (Promise b))) -> PrimIO (Promise b)

raw__then : Promise a -> (a -> JSIO (NS I [b, Promise b])) -> JSIO (Promise b)
raw__then p k = primIO $ prim__then p $ \x =>
  toPrim $ eitherT (\err => assert_total $ idris_crash $ dispErr err) (pure . toUnion2) $ k x

infixl 10 `then_`

-- export
-- then_ : {auto prf : Elem r [b, Promise b]} -> Promise a -> (a -> JSIO r) -> JSIO (Promise b)
-- then_ {b = b} {r = r} {prf = prf} p k = raw__then p $ \x => inj <$> k x
--   where
--     inj : r -> NS I [b, Promise b]
--     inj x = inject x {prf = prf}

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

%foreign "javascript:lambda:x => new Uint8Array(x)"
prim__toUint8Array : ArrayBuffer -> UInt8Array

export
Cast ArrayBuffer UInt8Array where
  cast = prim__toUint8Array
