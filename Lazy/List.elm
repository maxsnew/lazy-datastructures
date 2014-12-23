module Lazy.List where

import Basics    
import Lazy (Lazy)
import Lazy as L
import List ((::))
import List
import Trampoline as T        

type List a = Nil
            | Cons a (Lazy (List a))
type alias LList a = Lazy (List a)

cons : a -> Lazy (List a) -> Lazy (List a)
cons x xs = L.lazy (\_ -> Cons x xs)

nil : Lazy (List a)
nil = L.lazy (\_ -> Nil)             

foldr : (a -> Lazy b -> Lazy b) -> Lazy b -> LList a -> Lazy b
foldr f def l =
    l `L.andThen` \xs ->
        case xs of
          Nil       -> def
          Cons x xs -> f x (foldr f def xs)

foldl : (a -> b -> b) -> b -> LList a -> b
foldl f def l =
    let loop fuel acc l = if fuel <= 0
                          then T.Continue (\_ -> loop defFuel acc l)
                          else case L.force l of
                                 Nil       -> T.Done acc
                                 Cons x xs -> loop (fuel - 1) (f x acc) xs
    in T.trampoline (loop defFuel def l)
                       
map : (a -> b) -> LList a -> LList b
map f = foldr (\x xs -> cons (f x) xs) nil

filter : (a -> Bool) -> LList a -> LList a
filter p = foldr (\x xs -> if p x
                           then cons x xs
                           else xs
                 ) nil

filterMap : (a -> Maybe b) -> LList a -> LList b
filterMap f = foldr (\x xs -> case f x of
                                Nothing -> xs
                                Just y  -> cons y xs
                    ) nil

append : LList a -> LList a -> LList a
append l1 l2 = foldr cons l2 l1

concat : LList (LList a) -> LList a
concat = foldr append nil

concatMap : (a -> LList b) -> LList a -> LList b
concatMap f = foldr (\x xs -> append (f x) xs) nil

cycle : a -> LList a -> LList a
cycle x xs = let knot = cons x (L.lazy (\_ -> L.force <| append xs knot)) in knot

tails : LList a -> LList (LList a)
tails l =
    l `L.andThen`
          \xs -> case xs of
                   Nil       -> cons nil nil
                   Cons x xs -> cons (cons x xs) (tails xs)

unfoldr : (b -> (a, (Maybe (Lazy b)))) -> Lazy b -> LList a
unfoldr f start =
    start `L.andThen` \b ->
        let (x, m) = f b
            tail   = case m of
                       Nothing -> nil
                       Just t  -> unfoldr f t
        in cons x tail

iterate : (a -> a) -> a -> LList a
iterate f x =
    unfoldr (\y -> let z = f y in (z, Just (L.lazy (\_ -> z)))) (L.lazy (\_ -> x))

-- approximate : Int -> LList a -> Basics.List a
approximate n l = case n of
                    0 -> []
                    _ -> case L.force l of
                           Nil -> []
                           Cons x xs -> x :: (approximate (n - 1) xs)
                           
defFuel = 50