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
