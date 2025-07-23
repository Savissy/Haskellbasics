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
