module Lazy.Memoize where

import Lazy (Lazy)
import Lazy as L
import List ((::))

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

result : Memo a c -> Memo b c -> Memo (Result a b) c
result (Memo m1) (Memo m2) =
    Memo { memo f =
               let lmemo = m1.memo (\x -> f (Err x))
                   rmemo = m2.memo (\x -> f (Ok  x))
               in \x -> case x of
                          Err x -> lmemo x
                          Ok  x -> rmemo x
         }

pair : Memo a (b -> c) -> Memo b c -> Memo (a, b) c
pair (Memo m1) (Memo m2) =
    Memo { memo f = let t = m1.memo (\x -> m2.memo (\y -> f (x, y)))
                    in \(x, y) -> t x y
         }

maybe : Memo a b -> Memo (Maybe a) b
maybe m =
    let memoizer = result unit m
    in Memo { memo f =
                  let t = memoize memoizer (\x ->
                                                case x of
                                                  Err _ -> f Nothing
                                                  Ok  x -> f (Just x)
                                           )
                  in \x -> case x of
                             Nothing -> t (Err ())
                             Just x  -> t (Ok x)
            }

list : Memo a (List a -> b) -> Memo (List a) b
list m =
    let knot = Memo {
                 memo f =
                     let t = memoize (maybe (pair m knot))
                                     (\x -> case x of
                                              Nothing -> f []
                                              Just (x,xs) -> f (x::xs)
                                     )
                     in \x -> case x of
                                []    -> t Nothing
                                x::xs -> t (Just (x,xs))
               }
    in knot

{-
-- Sanity checking speed:

ack m n = case (m, n) of
            (0, _) -> n + 1
            (_, 0) -> ack (m - 1) 1
            (_, _) -> ack (m - 1) (ack m (n - 1))

slowBoolShow : Bool -> Int
slowBoolShow b = if b
                 then ack 3 9
                 else ack 3 9

fastbshow = memoize bool slowBoolShow

sbs' : (Bool, Bool) -> Int
sbs' = memoize (pair bool bool) (\(b1, b2) -> fastbshow b1 + fastbshow b2)
-}