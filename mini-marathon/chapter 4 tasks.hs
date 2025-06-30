-- Haskell Mini Marathons: Introduction to Typeclasses

-- MARATHON 1: Eq Typeclass Example

module TypeclassMarathon1 where
data Light = On | Off deriving (Eq)

main :: IO ()
main = print $ On == Off

-- MARATHON 2: Ord Typeclass Example

module TypeclassMarathon2 where
data Rank = Low | Medium | High deriving (Eq, Ord, Show)

main :: IO ()
main = print $ Medium < High

-- MARATHON 3: Show Typeclass

module TypeclassMarathon3 where
data Color = Red | Green | Blue deriving (Show)

main :: IO ()
main = print Red

-- MARATHON 4: Read Typeclass

module TypeclassMarathon4 where
data Status = Ok | Error deriving (Read, Show)

main :: IO ()
main = print (read "Ok" :: Status)

-- MARATHON 5: Custom Eq Implementation

module TypeclassMarathon5 where
data Switch = Up | Down

instance Eq Switch where
  Up == Up = True
  Down == Down = True
  _ == _ = False

main :: IO ()
main = print $ Up == Down

-- MARATHON 6: Custom Show Implementation

module TypeclassMarathon6 where
data Weather = Sunny | Rainy

instance Show Weather where
  show Sunny = "It is sunny!"
  show Rainy = "Rain is falling."

main :: IO ()
main = print Rainy

-- MARATHON 7: Typeclass Constraint in Function

module TypeclassMarathon7 where
compareVals :: (Eq a) => a -> a -> String
compareVals x y = if x == y then "Same" else "Different"

main :: IO ()
main = print $ compareVals 4 4

-- MARATHON 8: Deriving Multiple Typeclasses

module TypeclassMarathon8 where
data Mood = Happy | Sad deriving (Eq, Ord, Show, Read)

main :: IO ()
main = print $ read "Happy" < (Sad :: Mood)

-- MARATHON 9: Typeclass Inference

module TypeclassMarathon9 where
main :: IO ()
main = print $ show (max 3 7)

-- MARATHON 10: Numeric Typeclass Example

module TypeclassMarathon10 where
square :: Num a => a -> a
square x = x * x

main :: IO ()
main = print $ square 5.5

-- MARATHON 11: Enum Example

module TypeclassMarathon11 where
data Step = One | Two | Three deriving (Enum, Show)

main :: IO ()
main = print $ [One .. Three]

-- MARATHON 12: Bounded Example

module TypeclassMarathon12 where
data Size = Small | Medium | Large deriving (Bounded, Enum, Show)

main :: IO ()
main = print (minBound :: Size, maxBound :: Size)

-- MARATHON 13: Combining Eq and Show

module TypeclassMarathon13 where
data SwitchState = On | Off deriving (Eq, Show)

isSame :: SwitchState -> SwitchState -> String
isSame a b = if a == b then show a else "Different states"

main :: IO ()
main = print $ isSame On Off

-- MARATHON 14: Function Constraint in List

module TypeclassMarathon14 where
describeAll :: Show a => [a] -> [String]
describeAll = map show

main :: IO ()
main = print $ describeAll [1, 2, 3]

-- MARATHON 15: Read and Show Interaction

module TypeclassMarathon15 where
data Direction = North | South | East | West deriving (Read, Show)

main :: IO ()
main = print (read "East" :: Direction)

-- MARATHON 16: Deriving vs Manual Show

module TypeclassMarathon16 where
data Pet = Dog | Cat deriving Show

main :: IO ()
main = print Dog

-- MARATHON 17: Custom Ord Instance

module TypeclassMarathon17 where
data Grade = C | B | A deriving (Eq, Show)

instance Ord Grade where
  compare C C = EQ
  compare C _ = LT
  compare B C = GT
  compare B B = EQ
  compare B A = LT
  compare A A = EQ
  compare A _ = GT

main :: IO ()
main = print $ A > B

-- MARATHON 18: Typeclass with Generics

module TypeclassMarathon18 where
identity :: Eq a => a -> a -> Bool
identity x y = x == y

main :: IO ()
main = print $ identity "hello" "hello"

-- MARATHON 19: Use of Show in IO

module TypeclassMarathon19 where
report :: Show a => a -> IO ()
report val = putStrLn ("The value is: " ++ show val)

main :: IO ()
main = report 42

-- MARATHON 20: Tuple Equality Check

module TypeclassMarathon20 where
checkPair :: (Eq a, Eq b) => (a, b) -> (a, b) -> Bool
checkPair p1 p2 = p1 == p2

main :: IO ()
main = print $ checkPair (1, 'a') (1, 'a')
