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

> unary :: Bin -> Nat
> unary N = Z
> unary (O s) = double $ unary s
> unary (I s) = S $ double $ unary s

> double :: Nat -> Nat
> double Z = Z
> double (S n) = S $ S $ double n

> binary :: Nat -> Bin
> binary Z = N
> binary (S x) = succBin $ binary x

> succBin :: Bin -> Bin
> succBin N = I $ N
> succBin (O bs) = I $ bs
> succBin (I bs) = O $ succBin bs

exercise 1.2
============

> toList :: Sequ elem -> List elem
> toList Nil = Zero
> toList (OCons s) = flatten $ toList s
> toList (ICons e s) = Succ e $ flatten $ toList s

> flatten :: List (Pair elem) -> List elem
> flatten Zero = Zero
> flatten (Succ (a, b) n) = Succ a $ Succ b $ flatten n

> fromList :: List elem -> Sequ elem
> fromList Zero = Nil
> fromList (Succ elem list) = addSequ elem $ fromList list

> addSequ :: elem -> Sequ elem -> Sequ elem
> addSequ e Nil = ICons e Nil
> addSequ e (OCons s) = ICons e s
> addSequ e (ICons elem s) = OCons $ addSequ (e, elem) s
