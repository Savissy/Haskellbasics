Haskell key Typeclasses, Function Type Signatures and their Instances



 Semigroup

A Semigroup is an algebraic structure consisting of a set equipped with an associative binary operation. In Haskell, this is represented by the <> operator.



Type Signature:

haskell

class Semigroup a where

  (<>) :: a -> a -> a



Instances:

1. Lists (concatenation):

   haskell

   instance Semigroup [a] where

     (<>) = (++)

    Example: [1,2] <> [3,4] = [1,2,3,4]

2. Sum (addition for numbers):

   haskell

   newtype Sum a = Sum a

   instance Num a => Semigroup (Sum a) where

     Sum x <> Sum y = Sum (x + y)

    Example: Sum 5 <> Sum 10 = Sum 15

3. Max (keep the larger value):

   haskell

   newtype Max a = Max a

   instance Ord a => Semigroup (Max a) where

     Max x <> Max y = Max (max x y)

    Example: Max 3 <> Max 7 = Max 7



 Monoid

A Monoid is a Semigroup with an identity element (mempty), such that appending mempty to any value leaves it unchanged.



Type Signature:

haskell

class Semigroup a => Monoid a where

  mempty :: a

Instances:

1. Lists (empty list is identity):

   haskell

   instance Monoid [a] where

     mempty = []

    Example: [1,2] <> [] = [1,2]

2. Sum (0 is identity):

   haskell

   instance Num a => Monoid (Sum a) where

     mempty = Sum 0

    Example: Sum 5 <> Sum 0 = Sum 5

3. All (logical AND with True as identity):

   haskell

   newtype All = All Bool

   instance Monoid All where

     mempty = All True

     All x <> All y = All (x && y)

    Example: All True <> All True = All True



 Functor

A Functor is a typeclass for types that can be mapped over. It generalizes the map operation on lists.



Type Signature:

haskell

class Functor f where

  fmap :: (a -> b) -> f a -> f b

Instances:

1. Maybe (apply to Just, ignore Nothing):

   haskell

   instance Functor Maybe where

     fmap _ Nothing  = Nothing

     fmap f (Just x) = Just (f x)

    Example: fmap (+1) (Just 5) = Just 6

2. List (apply to each element):

   haskell

   instance Functor [] where

     fmap = map

    Example: fmap (2) [1,2,3] = [2,4,6]

3. Either e (apply to the Right case):

   haskell

   instance Functor (Either e) where

     fmap _ (Left x)  = Left x

     fmap f (Right y) = Right (f y)

    Example: fmap (+1) (Right 10) = Right 11

 

 Applicative

An Applicative Functor extends Functors with the ability to apply functions embedded within a context to values in the same context.



Type Signatures:

haskell

class Functor f => Applicative f where

  pure :: a -> f a

  (<>) :: f (a -> b) -> f a -> f b

Instances:

1. Maybe (apply if both function and value are Just):

   haskell

   instance Applicative Maybe where

     pure = Just

     Just f <> Just x = Just (f x)

     _ <> _ = Nothing

    Example: Just (+1) <> Just 5 = Just 6

2. List (apply every function to every value):

   haskell

   instance Applicative [] where

     pure x = [x]

     fs <> xs = [f x | f <- fs, x <- xs]

    Example: [(+1), (2)] <> [1,2] = [2,3,2,4]

3. Either e (apply if both are Right):

   haskell

   instance Applicative (Either e) where

     pure = Right

     Left e <> _ = Left e

     Right f <> r = fmap f r

    Example: Right (+1) <> Right 5 = Right 6

  

 Monad

A Monad extends Applicatives with the ability to sequence computations using >>= (bind), where the result of one computation can affect the next.



Type Signature:

haskell

class Applicative m => Monad m where

  (>>=) :: m a -> (a -> m b) -> m b

Instances:

1. Maybe (chain computations that may fail):

   haskell

   instance Monad Maybe where

     Nothing >>= _ = Nothing

     Just x >>= f = f x

    Example: Just 5 >>= \x -> Just (x+1) = Just 6

2. List (chain computations that produce multiple values):

   haskell

   instance Monad [] where

     xs >>= f = concat (map f xs)

    Example: [1,2] >>= \x -> [x, x+1] = [1,2,2,3]

3. Either e (chain computations that may error):

   haskell

   instance Monad (Either e) where

     Left e >>= _ = Left e

     Right x >>= f = f x

    Example: Right 5 >>= \x -> Right (x+1) = Right 6



Custom Typeclasses and Their Instances



 1. Printable

Types that can be converted to a custom string representation.



haskell

class Printable a where

    toCustomString :: a -> String



instance Printable Bool where

    toCustomString True = "Yes"

    toCustomString False = "No"



instance Printable Int where

    toCustomString n = "Number: " ++ show n



instance Printable [Char] where

    toCustomString s = "Text: " ++ s





 2. Emptiable

Types that can be checked for emptiness.



haskell

class Emptiable a where

    isEmpty :: a -> Bool



instance Emptiable [a] where

    isEmpty [] = True

    isEmpty _  = False



instance Emptiable (Maybe a) where

    isEmpty Nothing  = True

    isEmpty (Just _) = False



instance Emptiable Bool where

    isEmpty False = True

    isEmpty True  = False





 3. Duplicable

Types that can duplicate their content.



haskell

class Duplicable a where

    duplicate :: a -> a



instance Duplicable Int where

    duplicate x = x * 2



instance Duplicable String where

    duplicate s = s ++ s



instance Duplicable [a] where

    duplicate xs = xs ++ xs





 4. Defaultable

Types that have a default value.



haskell

class Defaultable a where

    defaultVal :: a



instance Defaultable Int where

    defaultVal = 0



instance Defaultable Bool where

    defaultVal = False



instance Defaultable [a] where

    defaultVal = []





 5. Reversible

Types that can be reversed.



haskell

class Reversible a where

    reverseIt :: a -> a



instance Reversible String where

    reverseIt = reverse



instance Reversible [a] where

    reverseIt = reverse



instance Reversible (a, b) where

    reverseIt (x, y) = (y, x)



