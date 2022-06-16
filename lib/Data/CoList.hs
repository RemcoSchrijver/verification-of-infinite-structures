module Data.CoList where

import Data.Thunk

import Data.Nat

data CoList a = Nil
              | (:::) a (Thunk (CoList a))

repeatCoList :: a -> CoList a
repeatCoList x
  = x :::
      \case
          Data.Thunk.Thunk.force -> repeatCoList x

fibonacciCoList :: Nat -> Nat -> CoList Nat
fibonacciCoList n1 n2
  = n1 :::
      \case
          Data.Thunk.Thunk.force -> n2 :::
                                      \case
                                          Data.Thunk.Thunk.force -> fibonacciCoList n2 (n1 +++ n2)

hdCoList :: CoList a -> Maybe a
hdCoList Nil = Nothing
hdCoList (x ::: xs) = Just x

tlCoList :: CoList a -> CoList a
tlCoList Nil = Nil
tlCoList (x ::: xs) = force xs

(!!!) :: CoList a -> Nat -> Maybe a
Nil !!! _ = Nothing
(x ::: xs) !!! Zero = Just x
(x ::: xs) !!! Suc n = force xs !!! n

takeCoList :: Nat -> CoList a -> [a]
takeCoList _ Nil = []
takeCoList Zero xs = []
takeCoList (Suc n) (x ::: xs) = x : takeCoList n (force xs)

dropCoList :: Nat -> CoList a -> CoList a
dropCoList _ Nil = Nil
dropCoList Zero xs = xs
dropCoList (Suc n) (x ::: xs) = dropCoList n (force xs)
