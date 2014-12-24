module Lazy.Memoize where

import Lazy (Lazy)
import Lazy as L

type Memo a b = Memo { memo : (a -> b) -> a -> b}

memoize : Memo a b -> (a -> b) -> a -> b
memoize (Memo m) = m.memo

unit : Memo () b
unit = Memo { memo f = let l = L.lazy f in \_ -> (L.force l) }
     
bool : Memo Bool b
bool =
    Memo { memo f = boolUntrie (boolTrie f) }

type alias BoolTrie a = (Lazy a, Lazy a)
boolTrie : (Bool -> b) -> BoolTrie b
boolTrie f = (L.lazy (\_ -> f True), L.lazy (\_ -> f False))

boolUntrie : BoolTrie a -> Bool -> a
boolUntrie (tt, ft) b = case b of
                          True  -> L.force tt
                          False -> L.force ft

ord : Memo Order b
ord = Memo { memo f = ordUntrie (ordTrie f) }

type alias OrderTrie a = (Lazy a, Lazy a, Lazy a)
ordTrie : (Order -> b) -> OrderTrie b
ordTrie f = (L.lazy (\_ -> f LT), L.lazy (\_ -> f EQ), L.lazy (\_ -> f GT))

ordUntrie : OrderTrie a -> Order -> a
ordUntrie (lt, eq, gt) b = case b of
                             LT -> L.force lt
                             EQ -> L.force eq
                             GT -> L.force gt

