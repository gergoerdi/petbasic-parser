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
