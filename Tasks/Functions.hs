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
