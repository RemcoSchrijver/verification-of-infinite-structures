module Data.InfiniteList where

import Data.Nat

data InfiniteList a = InfiniteList{hd :: a, tl :: InfiniteList a}

infNatList :: Nat -> InfiniteList Nat
infNatList n Data.InfiniteList.InfiniteList.hd = n
infNatList n Data.InfiniteList.InfiniteList.tl = infNatList (Suc n)

repeatInf :: a -> InfiniteList a
repeatInf x Data.InfiniteList.InfiniteList.hd = x
repeatInf x Data.InfiniteList.InfiniteList.tl = repeatInf x

fibonacci :: Nat -> Nat -> InfiniteList Nat
fibonacci n1 n2 Data.InfiniteList.InfiniteList.hd = n1
fibonacci n1 n2 Data.InfiniteList.InfiniteList.tl
  = fibonacci n2 (n1 +++ n2)

hdInf :: InfiniteList a -> a
hdInf list = hd list

tlInf :: InfiniteList a -> InfiniteList a
tlInf list = tl list

(!!!) :: InfiniteList a -> Nat -> a
list !!! Zero = hdInf list
list !!! Suc n = tlInf list !!! n

takeInf :: Nat -> InfiniteList a -> [a]
takeInf Zero list = []
takeInf (Suc n) list = hdInf list : takeInf n (tlInf list)

dropInf :: Nat -> InfiniteList a -> InfiniteList a
dropInf Zero list = list
dropInf (Suc n) list = dropInf n (tlInf list)

evenInf :: InfiniteList a -> InfiniteList a
evenInf xs Data.InfiniteList.InfiniteList.hd = hd xs
evenInf xs Data.InfiniteList.InfiniteList.tl = evenInf (tl (tl xs))

oddInf :: InfiniteList a -> InfiniteList a
oddInf xs = evenInf (tl xs)

splitInf :: InfiniteList a -> (InfiniteList a, InfiniteList a)
splitInf xs = (evenInf xs, oddInf xs)

mergeInf :: (InfiniteList a, InfiniteList a) -> InfiniteList a
mergeInf (xs, ys) Data.InfiniteList.InfiniteList.hd = hd xs
mergeInf (xs, ys) Data.InfiniteList.InfiniteList.tl
  = mergeInf (ys, tl xs)

mapInf :: InfiniteList a -> (a -> b) -> InfiniteList b
mapInf list f Data.InfiniteList.InfiniteList.hd = f (hd list)
mapInf list f Data.InfiniteList.InfiniteList.tl = mapInf list f

