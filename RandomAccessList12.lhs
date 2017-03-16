> {-# LANGUAGE UnicodeSyntax #-}
>
> module RandomAccessList12
> where
> -- import Unicode

< data Sequ elem
<
< nil ∷ Sequ elem
< cons ∷ elem → Sequ elem → Sequ elem
< head ∷ Sequ elem → elem
< tail ∷ Sequ elem → Sequ elem
< (!) ∷ Sequ elem → Integer → elem

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Anna Tökés s1005628

> import Prelude hiding (head, tail, (!))

exercise 2
==========

> type Pair elem=(elem,elem)

> data Sequ elem
>    = Nil
>    | ICons elem (Sequ (Pair elem))
>    | IICons (Pair elem) (Sequ (Pair elem))
>    deriving (Show)

> nil :: Sequ elem
> nil = Nil

> cons :: elem -> Sequ elem -> Sequ elem
> cons a Nil = ICons a Nil
> cons a (ICons b s) = IICons (a,b) s
> cons a (IICons b s) = ICons a (cons b s)

> head :: Sequ elem -> elem
> head Nil = error "Empty seq"
> head (ICons e _) = e
> head (IICons(e1,e2) _) = e1

> tail :: Sequ elem -> Sequ elem
> tail Nil = error "Empty seq"
> tail (IICons (_,b) s) = ICons b s
> tail (ICons _ t) = decr t

> decr :: Sequ (Pair elem) -> Sequ elem
> decr Nil = Nil
> decr (ICons a t) = IICons a (decr t)
> decr (IICons (a,b) t) = IICons a (ICons b t)

> (!) :: Sequ elem -> Integer -> elem
> Nil ! _ = error "Index out of bound"
> (ICons a _ ) ! 0 = a
> (IICons (a,_) _) ! 0 = a
> (IICons (_,b) _) ! 1 = b
> (ICons _ b) ! n = b ! ((n-1) `div` 2)  !.! ((n-1) `mod` 2)
> (IICons _ b) ! n = b ! ((n-2) `div` 2) !.! ((n-2) `mod` 2)

> (a1,_) !.! 0= a1
> (_,a2) !.! 1= a2

Why is it advantageous to use a zeroless numerical representation?
Because the {1,2}-binary number system is irredundant. The running
time of (!) is logarithmic and it's shorter then the (!) in
{0,1}-binary number system.
Does the running time depend on the index i or on the size of s?
It depends on the index, because it has to be divided until it's 0 or 1.
