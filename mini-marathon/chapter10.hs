--Define a type class Comparable with a function compareWith :: a -> a -> Ordering.
--Implement it for Blockchain.

data BlockChain = Block | Chain | Node | Central deriving (Show, Eq, Ord)

class Comparable a where
 compareWith :: a -> a -> Ordering
 
instance Comparable BlockChain where
 compareWith Block Chain = GT
 compareWith Node Central = LT
 
blockChain :: BlockChain -> BlockChain -> Ordering
blockChain Block Chain = compareWith Block Chain
blockChain Node Central = compareWith Node Central

main :: IO ()
main = do
  print $ blockChain Block Chain
  print $ blockChain Node Central

--Define a new type class ShowSimple that requires a function showSimple :: a -> String for simple string conversion.
--Implement an instance for PaymentMethod type.

data PaymentMethod = Cash | Card | Cc deriving Show

class ShowSimple a where
 showSimple :: a -> String
 
instance ShowSimple PaymentMethod where
 showSimple Cash = "Cash"
 showSimple Card = "Card"
 showSimple Cc = "Cc"
 
display :: PaymentMethod -> String
display Cash = showSimple Cash
display Card = showSimple Card
display Cc = showSimple Cc

main :: IO ()
main = do
  print $ display Cash
  print $ display Card
  print $ display Cc

--Create a type class Summable that provides sumUp :: [a] -> a.
--Implement it for the type Int

class Summable a where
 sumUp :: [a] -> Int
 
instance Summable Int where
 sumUp [] = 0
 sumUp (x:xs) = x + sumUp xs
 
addUp :: [Int] -> Int
addUp x = sumUp x


main :: IO ()
main = do
  print $ addUp [1 .. 10]

--Create a parameterized type Box a and make it an instance of Eq.

data Box a = Empty | Has a deriving Show

instance Eq a => Eq (Box a) where
 (Has x) == Empty = False
 (Has x) == (Has y) = x == y
 Empty == Empty = True
 _ == _ = False
 
box :: Eq a => Box a -> Box a -> Bool
box a b = a == b

main :: IO ()
main = do
 print $ box (Has 1) Empty
 print $ box (Has 1) (Has 2)
 print $ box (Empty :: Box Int) (Empty :: Box Int)

--Define a type class ShowDetailed with a function showDetailed :: a -> String.
--Implement it for a type User.

data User = Name | Age | Sex deriving Show

class ShowDetailed a where
 showDetailed :: a -> String
 
instance ShowDetailed User where
 showDetailed Name = "Saviour"
 showDetailed Age = "Twenty-Six"
 showDetailed Sex = "Male"
 
details :: User -> String
details x = showDetailed x

main :: IO ()
main = do
 print $ details Name
 print $ details Age
 print $ details Sex

class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq Int where
 compareEquality x y = x == y
 
compareNum :: Int -> Int -> Bool
compareNum x y = compareEquality x y
 

main :: IO ()
main = do
  print $ compareNum 4 4

