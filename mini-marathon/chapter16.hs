import Data.Char (toUpper)
import qualified Data.Map as Map

-- HC16T1: Reverse a String
reverseString :: String -> String
reverseString = reverse

main1 :: IO ()
main1 = do
    putStrLn "HC16T1: Reverse a String"
    putStrLn $ reverseString "Hello"

-- HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

main2 :: IO ()
main2 = do
    putStrLn "HC16T2: Palindrome Checker"
    print $ isPalindrome "racecar"
    print $ isPalindrome "hello"

-- HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main3 :: IO ()
main3 = do
    putStrLn "HC16T3: Factorial"
    print $ factorial 5

-- HC16T4: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

main4 :: IO ()
main4 = do
    putStrLn "HC16T4: Filter Even Numbers"
    print $ filterEven [1..10]

-- HC16T5: Uppercase String
toUpperCase :: String -> String
toUpperCase = map toUpper

main5 :: IO ()
main5 = do
    putStrLn "HC16T5: Uppercase String"
    putStrLn $ toUpperCase "hello world"

-- HC16T6: nth Fibonacci Number
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main6 :: IO ()
main6 = do
    putStrLn "HC16T6: nth Fibonacci Number"
    print $ fibonacci 10

-- HC16T7: Element Existence in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

main7 :: IO ()
main7 = do
    putStrLn "HC16T7: Element Existence in List"
    print $ elementExists 3 [1, 2, 3, 4]
    print $ elementExists 'a' "hello"

-- HC16T8: Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
        | y <= z    = y : z : zs
        | otherwise = z : insert y zs

main8 :: IO ()
main8 = do
    putStrLn "HC16T8: Insertion Sort"
    print $ insertionSort [5, 2, 9, 1, 5, 6]

-- HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise   = x : removeDuplicates xs

main9 :: IO ()
main9 = do
    putStrLn "HC16T9: Remove Duplicates from List"
    print $ removeDuplicates [1, 2, 2, 3, 4, 4, 5]

-- HC16T10: Character Frequency in String
charFrequency :: String -> Map.Map Char Int
charFrequency str = Map.fromListWith (+) [(c, 1) | c <- str]

main10 :: IO ()
main10 = do
    putStrLn "HC16T10: Character Frequency in String"
    print $ charFrequency "hello world"
