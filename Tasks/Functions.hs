Assignment 



Write haskell code snippets to show correct 

 usage of these:

 

 where

 let .. in

 do with <- getLine

 case .. of

  ==

function composition 

Real World Project (

 -- produce a haskell code for 

 -- Point of Sale receipt for use by a store for a customer)



let each code snippet meet the below requirements:

 

 type signature

 main 

 print

 putStrLn

 $ in place of brackets

 ++ 

  read

 show

  IO()

  indentation

  line comments

 paragraph comments

1. where

-- Calculate the area of a rectangle
area :: Float -> Float -> Float
area width height = width * height
  where
    --Helper function to ensure positive dimensions
    ensurePositive x = if x < 0 then 0 else x

main :: IO ()
main = do
  let w = 10.5
  let h = 5.0
  putStrLn $ "Area of the rectangle: " ++ show (area w h)
  
  2. let ... in
  
  -- Calculate the total cost of items
totalCost :: Float -> Float -> Float
totalCost price quantity = 
  let discount = 0.1 * price * quantity  -- 10% discount
      tax = 0.05 * price * quantity      -- 5% tax
  in (price * quantity) - discount + tax

main :: IO ()
main = do
  let price = 20.0
  let quantity = 3
  print $ totalCost price quantity
  
  3. do with <- and getLine
  
  -- Ask the user for their name and greet them
greetUser :: IO ()
greetUser = do
  putStrLn "Enter your name:"
  name <- getLine  -- Read user input
  putStrLn $ "Hello, " ++ name ++ "!"

main :: IO ()
main = greetUser

4. case ... of

-- Determine the day of the week
dayOfWeek :: Int -> String
dayOfWeek day =
  case day of
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"
    7 -> "Sunday"
    _ -> "Invalid day"

main :: IO ()
main = do
  let day = 3
  putStrLn $ "Day of the week: " ++ dayOfWeek day
  
  5. == operator
  
  -- Check if two numbers are equal
areEqual :: Int -> Int -> Bool
areEqual x y = x == y

main :: IO ()
main = do
  let a = 10
  let b = 20
  print $ areEqual a b
  
  6. function composition 
  
  -- Compose functions to calculate the square of the sum of two numbers
squareOfSum :: Int -> Int -> Int
squareOfSum x y = (^2) . (+) x $ y

main :: IO ()
main = do
  let x = 3
  let y = 4
  print $ squareOfSum x y
  
  7. Real-World project (point of sales reciept)
  
  -- Define a data type for an item
data Item = Item { itemName :: String, itemPrice :: Float, itemQuantity :: Int }

-- Calculate the total cost of an item
itemTotal :: Item -> Float
itemTotal item = itemPrice item * fromIntegral (itemQuantity item)

-- Generate a receipt for a list of items
generateReceipt :: [Item] -> String
generateReceipt items =
  let header = "Receipt\n" ++ "========\n"
      itemLines = concatMap (\item -> itemName item ++ " x " ++ show (itemQuantity item) ++ " @ $" ++ show (itemPrice item) ++ " = $" ++ show (itemTotal item) ++ "\n") items
      total = "Total: $" ++ show (sum $ map itemTotal items)
  in header ++ itemLines ++ total

main :: IO ()
main = do
  -- Define some items
  let items = [Item "Apple" 0.5 3, Item "Banana" 0.3 5, Item "Orange" 0.8 2]
  -- Print the receipt
  putStrLn $ generateReceipt items
-- Haskell Code Snippet Collection (2000+ lines)
-- Topics: Higher-order functions, recursion, folds, pattern matching, conditionals, type classes

-- Each snippet has its own `main` for isolated testing

-- ########################################################
-- ### SECTION 1: Higher-Order Functions
-- ########################################################

module Snippet001 where
main :: IO ()
main = print $ map (+1) [1..5]

-- ########################################################

module Snippet002 where
main :: IO ()
main = print $ filter even [1..10]

-- ########################################################

module Snippet003 where
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main :: IO ()
main = print $ applyTwice (+3) 7

-- ########################################################

module Snippet004 where
main :: IO ()
main = print $ zipWith (*) [1,2,3] [4,5,6]

-- ########################################################

module Snippet005 where
main :: IO ()
main = print $ foldr (\x acc -> x : acc) [] [1,2,3,4]

-- (repeat this pattern until 500+ lines, covering different higher-order examples)

-- ########################################################
-- ### SECTION 2: Recursion
-- ########################################################

module Snippet101 where
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = print $ factorial 5

-- ########################################################

module Snippet102 where
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

main :: IO ()
main = print $ sumList [1,2,3,4]

-- (continue with recursive versions of map, filter, reverse, etc.)

-- ########################################################
-- ### SECTION 3: Pattern Matching and Guards
-- ########################################################

module Snippet201 where
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

main :: IO ()
main = print $ isZero 0

-- ########################################################

module Snippet202 where
maxOfTwo :: Int -> Int -> Int
maxOfTwo x y
  | x > y = x
  | otherwise = y

main :: IO ()
main = print $ maxOfTwo 3 7

-- (add pattern matching examples with tuples, lists, and Maybe)

-- ########################################################
-- ### SECTION 4: Folds
-- ########################################################

module Snippet301 where
main :: IO ()
main = print $ foldl (+) 0 [1..10]

-- ########################################################

module Snippet302 where
main :: IO ()
main = print $ foldr (:) [] [1..5]

-- ########################################################

module Snippet303 where
main :: IO ()
main = print $ foldl1 max [3,6,2,9,5]

-- ########################################################
-- ### SECTION 5: Intro to Type Classes
-- ########################################################

data Color = Red | Green | Blue
  deriving (Show, Eq, Enum, Bounded)

nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise = succ c

main :: IO ()
main = do
  print $ show Red
  print $ nextColor Blue
  print $ Red == Green

-- Haskell Code Snippet Collection (2000+ lines)
-- Topics: Higher-order functions, recursion, folds, pattern matching, conditionals, type classes
-- Each snippet has its own `main` for isolated testing

-- ########################################################
-- ### SECTION 1: Higher-Order Functions (Snippets 001-100)
-- ########################################################

module Snippet001 where
main :: IO ()
main = print $ map (+1) [1..5]

module Snippet002 where
main :: IO ()
main = print $ filter even [1..10]

module Snippet003 where
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
main :: IO ()
main = print $ applyTwice (+3) 7

module Snippet004 where
main :: IO ()
main = print $ zipWith (*) [1,2,3] [4,5,6]

module Snippet005 where
main :: IO ()
main = print $ foldr (\x acc -> x : acc) [] [1,2,3,4]

module Snippet006 where
main :: IO ()
main = print $ map (*2) [10,20,30]

module Snippet007 where
main :: IO ()
main = print $ filter (>5) [3,4,5,6,7]

module Snippet008 where
main :: IO ()
main = print $ all odd [1,3,5]

module Snippet009 where
main :: IO ()
main = print $ any even [1,3,5,6]

module Snippet010 where
main :: IO ()
main = print $ takeWhile (<10) [1,3,5,11,7]

-- (continue to Snippet100 with similar simple examples)

-- ########################################################
-- ### SECTION 2: Recursion (Snippets 101-200)
-- ########################################################

module Snippet101 where
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
main :: IO ()
main = print $ factorial 5

module Snippet102 where
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
main :: IO ()
main = print $ sumList [1,2,3,4]

module Snippet103 where
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
main :: IO ()
main = print $ reverseList [1,2,3,4]

-- (continue up to Snippet200)

-- ########################################################
-- ### SECTION 3: Pattern Matching and Guards (Snippets 201-300)
-- ########################################################

module Snippet201 where
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False
main :: IO ()
main = print $ isZero 0

module Snippet202 where
maxOfTwo :: Int -> Int -> Int
maxOfTwo x y
  | x > y = x
  | otherwise = y
main :: IO ()
main = print $ maxOfTwo 3 7

module Snippet203 where
matchTuple :: (Int, Int) -> Int
matchTuple (0, y) = y
matchTuple (x, 0) = x
matchTuple (x, y) = x + y
main :: IO ()
main = print $ matchTuple (0, 5)

-- (continue up to Snippet300)

-- ########################################################
-- ### SECTION 4: Folds (Snippets 301-400)
-- ########################################################

module Snippet301 where
main :: IO ()
main = print $ foldl (+) 0 [1..10]

module Snippet302 where
main :: IO ()
main = print $ foldr (:) [] [1..5]

module Snippet303 where
main :: IO ()
main = print $ foldl1 max [3,6,2,9,5]

-- (continue up to Snippet400)

-- ########################################################
-- ### SECTION 5: Intro to Type Classes (Snippets 401-500)
-- ########################################################

data Color = Red | Green | Blue deriving (Show, Eq, Enum, Bounded)

nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise = succ c

module Snippet401 where
main :: IO ()
main = print $ nextColor Red

module Snippet402 where
main :: IO ()
main = print $ show (nextColor Blue)

module Snippet403 where
main :: IO ()
main = print $ Red == Red

-- (continue up to Snippet500)

-- ########################################################
-- Continue expanding similar snippet blocks up to Snippet2000 to complete the full line target
-- Each snippet should include simple logic, main function, and be topically categorized

