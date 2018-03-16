import Data.Bifunctor
import Data.Functor.Const
import Data.Functor.Identity

{-- 1: Show that the data type:

data Pair a b = Pair a b

  is a bifunctor. For additional credit implement all three methods
  of Bifunctor and use equational reasoning to show that these definitions
  are compatible with the default implementations whenever they can be applied.
--}

data Pair a b = Pair a b

instance Bifunctor Pair where
  bimap g h (Pair a b) = Pair (g a) (h b)
  first g (Pair a b) = Pair (g a) b
  second h (Pair a b) = Pair a (h b)

{--
Default implementation:
  first g = bimap g id
This implementation:
  first g (Pair a b) = bimap g id (Pair a b) = Pair (g a) (id b) = Pair (g a) b

-}


{-- 2. Show the isomorphism between the standard definition of Maybe and this desugaring:

type Maybe' a = Either (Const () a) (Identity a)

Hint: Define two mappings between the two implementations. For additional credit,
 show that they are the inverse of each other using equational reasoning.

-}
type Maybe' a = Either (Const () a) (Identity a)

toMaybe' :: Maybe a -> Maybe' a
toMaybe' Nothing = Left (Const () a)
toMaybe' Just a = Right (Identity a)
