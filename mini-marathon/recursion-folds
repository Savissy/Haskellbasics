-- Haskell Recursion & Folds Mini Marathons

-- MARATHON 1: Recursive Sum

module RecFoldMarathon1 where
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

main :: IO ()
main = print $ sumList [1..5]

-- MARATHON 2: Recursive Product

module RecFoldMarathon2 where
productList :: [Int] -> Int
productList [] = 1
productList (x:xs) = x * productList xs

main :: IO ()
main = print $ productList [1..4]

-- MARATHON 3: Recursive Length

module RecFoldMarathon3 where
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

main :: IO ()
main = print $ myLength "hello"

-- MARATHON 4: Recursive Reverse

module RecFoldMarathon4 where
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

main :: IO ()
main = print $ myReverse [1..5]

-- MARATHON 5: Foldl Sum

module RecFoldMarathon5 where
foldSum :: [Int] -> Int
foldSum = foldl (+) 0

main :: IO ()
main = print $ foldSum [1..10]

-- MARATHON 6: Foldr Product

module RecFoldMarathon6 where
foldProduct :: [Int] -> Int
foldProduct = foldr (*) 1

main :: IO ()
main = print $ foldProduct [1..5]

-- MARATHON 7: Recursive Max

module RecFoldMarathon7 where
myMax :: [Int] -> Int
myMax [x] = x
myMax (x:xs) = max x (myMax xs)

main :: IO ()
main = print $ myMax [3, 6, 2, 8]

-- MARATHON 8: Foldr Max

module RecFoldMarathon8 where
foldMax :: [Int] -> Int
foldMax = foldr1 max

main :: IO ()
main = print $ foldMax [1, 4, 9, 2]

-- MARATHON 9: Map with Recursion

module RecFoldMarathon9 where
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

main :: IO ()
main = print $ myMap (+1) [1..5]

-- MARATHON 10: Filter with Recursion

module RecFoldMarathon10 where
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs

main :: IO ()
main = print $ myFilter even [1..10]

-- MARATHON 11: Foldr to Reverse

module RecFoldMarathon11 where
foldReverse :: [a] -> [a]
foldReverse = foldr (\x acc -> acc ++ [x]) []

main :: IO ()
main = print $ foldReverse [1..5]

-- MARATHON 12: Sum of Squares Fold

module RecFoldMarathon12 where
sumSquares :: [Int] -> Int
sumSquares = foldl (\acc x -> acc + x*x) 0

main :: IO ()
main = print $ sumSquares [1..4]

-- MARATHON 13: Count Elements with Fold

module RecFoldMarathon13 where
countFold :: [a] -> Int
countFold = foldr (\_ acc -> acc + 1) 0

main :: IO ()
main = print $ countFold "count me"

-- MARATHON 14: Fibonacci Recursively

module RecFoldMarathon14 where
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = print $ fib 10

-- MARATHON 15: Factorial Recursively

module RecFoldMarathon15 where
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

main :: IO ()
main = print $ factorial 5

-- MARATHON 16: Foldl to Concatenate Strings

module RecFoldMarathon16 where
concatStrings :: [String] -> String
concatStrings = foldl (++) ""

main :: IO ()
main = print $ concatStrings ["Hello", " ", "World"]

-- MARATHON 17: Recursively Check All True

module RecFoldMarathon17 where
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = x && allTrue xs

main :: IO ()
main = print $ allTrue [True, True, False]

-- MARATHON 18: Foldr and (all)

module RecFoldMarathon18 where
foldAll :: [Bool] -> Bool
foldAll = foldr (&&) True

main :: IO ()
main = print $ foldAll [True, True, True]

-- MARATHON 19: Foldr to Flatten List

module RecFoldMarathon19 where
flatten :: [[a]] -> [a]
flatten = foldr (++) []

main :: IO ()
main = print $ flatten [[1,2],[3,4],[5]]

-- MARATHON 20: Foldl to Build String

module RecFoldMarathon20 where
buildSentence :: [String] -> String
buildSentence = foldl1 (\acc word -> acc ++ " " ++ word)

main :: IO ()
main = print $ buildSentence ["Haskell","is","fun"]
