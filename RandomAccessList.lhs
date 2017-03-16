> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList
> where
> -- import Unicode

> data Nat = Z | S Nat
>   deriving(Show)

> data List elem = Zero | Succ elem (List elem)
>   deriving(Show)

> data Bin = N | O Bin | I Bin
>   deriving(Show)

> type Pair elem = (elem, elem)

> data Sequ elem
>   = Nil
>   | OCons (Sequ (Pair elem))
>   | ICons elem (Sequ (Pair elem))
>   deriving(Show)

< unary ∷ Bin → Nat
< binary ∷ Nat → Bin

< toList ∷ Sequ elem → List elem
< fromList ∷ List elem → Sequ elem

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Anna Tökés s1005628

exercise 1.1
============

exercise 1.2
============
