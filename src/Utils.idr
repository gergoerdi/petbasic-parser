module Utils

import JS
import Web.Dom

%foreign "browser:lambda:(_a, _b, x, y) => ((console.log(x),y))"
export traceConsole : a -> b -> b

export
traceConsoleId : a -> a
traceConsoleId x = traceConsole x x

export
zipFrom : Num a => a -> (1 xs : List b) -> List (a, b)
zipFrom i [] = []
zipFrom i (x :: xs) = (i, x) :: zipFrom (i + 1) xs

export
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
