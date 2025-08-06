main :: IO ()
main = do
  putStrLn "Welcome to Haskell Programming!"

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

main :: IO ()
main = do
  putStrLn "Enter the first number:"
  input1 <- getLine
  putStrLn "Enter the second number:"
  input2 <- getLine
  let x = read input1 :: Int
      y = read input2 :: Int
      result = addTwoNumbers x y
  putStrLn ("The sum is: " ++ show result)

addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

main :: IO ()
main = do
  let result = addTwoNumbers 5 7
  print result

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let n = read input :: Int
  print $ factorial n

isPalindrome :: String -> Bool
isPalindrome str
  | length str <= 1 = True
  | head str == last str = isPalindrome (init (tail str))
  | otherwise = False

main :: IO ()
main = do
  putStrLn "Enter a string:"
  input <- getLine
  print $ isPalindrome input

import Data.List (sort)

main :: IO ()
main = do
  putStrLn "Enter a list of integers separated by spaces:"
  input <- getLine
  let numbers = map read (words input) :: [Int]
  print $ sort numbers

calculateCircleArea :: Float -> Float
calculateCircleArea r = pi * r * r

main :: IO ()
main = do
  putStrLn "Enter the radius:"
  input <- getLine
  let radius = read input :: Float
  print $ calculateCircleArea radius

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x < y     = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

main :: IO ()
main = do
  putStrLn "Enter first sorted list:"
  l1 <- getLine
  putStrLn "Enter second sorted list:"
  l2 <- getLine
  let list1 = map read (words l1) :: [Int]
  let list2 = map read (words l2) :: [Int]
  print $ mergeLists list1 list2

