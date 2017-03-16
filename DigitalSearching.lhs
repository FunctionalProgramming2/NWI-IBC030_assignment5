> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeFamilyDependencies #-}

> module DigitalSearching
> where
> import Prelude hiding (lookup)
> import Data.Maybe
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

I though that this was a pair of maps from the exercise description but it is
apparently a map of maps which I think was not obvious.

> data instance Map (key1, key2) val = P (Map key1 (Map key2 val))

> instance (Key key1, Key key2) => Key (key1, key2) where
>   empty = P empty
>   insert (k1, k2) f (P m) = P $ insert k1 (pins k2 f) m
>   lookup (k1, k2) (P m) = lookup k1 m >>= lookup k2

> pins :: (Key key)
>   => key -> (Maybe val -> val) -> (Maybe (Map key val)) -> (Map key val)
> pins k f Nothing = insert k f empty
> pins k f (Just m) = insert k f m

exercise 3.3
============

> data instance Map [key] val = L (Map (List key) val)

> instance (Key key) => Key [key] where
>   empty = L empty
>   insert kl f (L m) = L $ insert (toList kl) f m
>   lookup kl (L m) = lookup (toList kl) m

type BTree elem = Either () (Tree elem, Tree elem)

What about other datatypes such as binary trees?

I assume that this question means: Can you use other data structures, such as
binary trees, as keys for Map instances?
I think it should be possible. Every map would have to have exactly two children
maps, as opposed to a natural number of children maps for lists, as we did
above.
