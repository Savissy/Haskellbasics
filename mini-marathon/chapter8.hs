-- HC8T1
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to val = "The value " ++ show val ++ " moved from " ++ from ++ " to " ++ to

main1 :: IO ()
main1 = do
  print $ generateTx "addr1" "addr2" 100

-- HC8T2
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person String (String, Int) PaymentMethod deriving Show

bob :: Person
bob = Person "Bob" ("Main Street", 12) Cash

main2 :: IO ()
main2 = print bob

-- HC8T3
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main3 :: IO ()
main3 = do
  print $ area (Circle 5)         -- 78.54...
  print $ area (Rectangle 10 5)   -- 50.0

-- HC8T4
data Employee = Employee { name :: String, experienceInYears :: Float } deriving Show

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

main4 :: IO ()
main4 = print richard

-- HC8T5
data PersonR = PersonR
  { name :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonR
person1 = PersonR { name = "Alice", age = 30, isEmployed = True }

person2 :: PersonR
person2 = PersonR { name = "Eve", age = 25, isEmployed = False }

main5 :: IO ()
main5 = do
  print person1
  print person2

-- HC8T6
data ShapeR
  = CircleR { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleR { width :: Float, height :: Float, color :: String }
  deriving Show

circleEx :: ShapeR
circleEx = CircleR { center = (0.0, 0.0), color = "Red", radius = 5.0 }

rectangleEx :: ShapeR
rectangleEx = RectangleR { width = 10.0, height = 4.0, color = "Blue" }

main6 :: IO ()
main6 = do
  print circleEx
  print rectangleEx

-- HC8T7
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

main7 :: IO ()
main7 = do
  let d = Dog "Rex"
  let c = Cat "Whiskers"
  putStrLn $ describeAnimal d
  putStrLn $ describeAnimal c

-- HC8T8
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello " ++ n ++ "! You are " ++ show a ++ " years old."

main8 :: IO ()
main8 = putStrLn $ greet "Sophie" 28

-- HC8T9
type Address = String
type Value = Int

data Transaction = Transaction
  { from :: Address
  , to :: Address
  , amount :: Value
  , transactionId :: String
  } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr val =
  let txId = "TX-" ++ show (length fromAddr + length toAddr + val)
  in show (Transaction fromAddr toAddr val txId)

main9 :: IO ()
main9 = putStrLn $ createTransaction "addr1" "addr2" 150

-- HC8T10
data Book = Book
  { title :: String
  , author :: String
  , year :: Int
  } deriving Show

myBook :: Book
myBook = Book "The Haskell Book" "Graham Hutton" 2016

main10 :: IO ()
main10 = print myBook
