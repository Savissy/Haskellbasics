import Data.Char (toLower)

mapToLower :: String -> String
mapToLower = fmap toLower

main :: IO ()
main = do
    putStrLn $ mapToLower "HELLO World!"  -- "hello world!"

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

main :: IO ()
main = do
    let tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
    print $ fmap (*2) tree

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

main :: IO ()
main = do
    let tree = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
    print $ incrementTreeValues tree

mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

main :: IO ()
main = do
    print $ mapToBits [True, False, True, True, False]  -- "10110"

data Either' a b = Left' a | Right' b
    deriving (Show)

instance Functor (Either' a) where
    fmap _ (Left' x)  = Left' x
    fmap f (Right' y) = Right' (f y)

main :: IO ()
main = do
    print $ fmap (+1) (Right' 5 :: Either' String Int)    -- Right' 6
    print $ fmap (+1) (Left' "err" :: Either' String Int) -- Left' "err"

applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

main :: IO ()
main = do
    print $ applyToMaybe (*2) (Just 4) -- Just 8
    print $ applyToMaybe (*2) Nothing  -- Nothing

identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == x

main :: IO ()
main = do
    print $ identityLawCheck [1, 2, 3]  -- True
    print $ identityLawCheck (Just "hi") -- True
