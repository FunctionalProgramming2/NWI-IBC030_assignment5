> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeFamilyDependencies #-}

> module DigitalSearching
> where
> import Prelude hiding (lookup)
> -- import Unicode

> data family Map key ∷ * → *
>
> class (Ord key) ⇒ Key key where
>   empty ∷ Map key val
>   insert ∷ key → (Maybe val → val) → Map key val → Map key val
>   lookup ∷ key → Map key val → Maybe val

> data instance Map () val = Empty | Single val
>
> instance Key () where
>   empty = Empty
>   insert () f (Empty) = Single (f Nothing)
>   insert () f (Single v) = Single (f (Just v))
>   lookup () (Empty) = Nothing
>   lookup () (Single v) = Just v

< data instance Map (Either key1 key2) val =
<
< instance (Key key1, Key key2) ⇒ Key (Either key1 key2) where

< data instance Map (key1, key2) val =
<
< instance (Key key1, Key key2) ⇒ Key (key1, key2) where

> type List elem = Either () (elem, [elem])
>
> toList ∷ [elem] → List elem
> toList [] = Left ()
> toList (a : as) = Right (a, as)

< data instance Map [key] val =
<
< instance (Key key) ⇒ Key [key] where

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Anna Tökés s1005628

exercise 3.1
============

> data instance Map (Either key1 key2) val = E (Map key1 val) (Map key2 val)

> instance (Key key1, Key key2) => Key (Either key1 key2) where
>   empty = E empty empty
>   insert (Left k) f (E m1 m2) = E (insert k f m1) m2
>   insert (Right k) f (E m1 m2) = E m1 (insert k f m2)
>   lookup (Left k) (E m1 m2) = lookup k m1
>   lookup (Right k) (E m1 m2) = lookup k m2

exercise 3.2
============

exercise 3.3
============
