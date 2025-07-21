data Box a = Empty | Has a deriving Show

box :: Num a => Box a
box = Has 2

main :: IO ()
main = do
  print $ box 

data Box a = Empty | Has a deriving Show

addN :: Num a => a -> Box a -> a
addN x Empty = x
addN x (Has y) = x + y  

main :: IO ()
main = do
  print $ addN 2 (Has 3)
data Box a = Empty | Has a deriving Show

extract :: Num a => a -> Box a -> a
extract x Empty = x
extract _ (Has y) = y  

main :: IO ()
main = do
  print $ extract 2 Empty 
  print $ extract 2 (Has 3)

data Shape a = Circle
 {
   color :: a
 }
 | Rectangle
 {
   color :: a
 } deriving Show

shapeColor :: Shape String
shapeColor = Circle {color="Red"}
shapeColor1 :: Shape String
shapeColor1 = Rectangle {color="Green"}

main :: IO ()
main = do
  print $ shapeColor  
  print $ shapeColor1 

data Tweet = Tweet
 {
   content :: String
   , likes :: Int
   , comments :: Tweet
 }
 | Reply
 {
  response :: String
 } deriving Show

tweet :: Tweet
tweet = Tweet {content="Nigeria is great", likes=2, comments= Reply {response = "Thank you"}}

main :: IO ()
main = do
  print $ tweet 

data Sequence a = Empty | a :-> (Sequence a) deriving Show

linearSeq :: Eq a => a -> Sequence a -> Bool
linearSeq _ Empty = False
linearSeq x (y :-> ys) = x == y || linearSeq x ys

arrToSeq :: Num a => [a] -> Sequence a
arrToSeq [] = Empty
arrToSeq (x:xs) = x :-> arrToSeq xs

main :: IO ()
main = do
  let seq1 = arrToSeq [1 .. 6]
  print $ linearSeq 2 seq1
  print $ linearSeq 7 seq1
