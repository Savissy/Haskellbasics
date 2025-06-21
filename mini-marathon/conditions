-- Haskell Code Snippet Collection (Conditions & Helper Constructions Marathon)

-- MARATHON 1: Absolute Value (using guards)

module Marathon1 where
myAbs :: Int -> Int
myAbs x
  | x < 0     = -x
  | otherwise = x

main :: IO ()
main = print $ myAbs (-10)

-- MARATHON 2: Absolute Value (using if)

module Marathon2 where
myAbs :: Int -> Int
myAbs x = if x < 0 then -x else x

main :: IO ()
main = print $ myAbs (-20)

-- MARATHON 3: Even or Odd

module Marathon3 where
evenOrOdd :: Int -> String
evenOrOdd n = if even n then "Even" else "Odd"

main :: IO ()
main = print $ evenOrOdd 7

-- MARATHON 4: Grade Classifier

module Marathon4 where
grade :: Int -> String
grade score
  | score < 0 || score > 100 = "Invalid"
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

main :: IO ()
main = print $ grade 85

-- MARATHON 5: Compare Three

module Marathon5 where
compareThree :: Int -> Int -> Int -> String
compareThree x y z
  | x == y && y == z = "All equal"
  | x == y || y == z || x == z = "Two equal"
  | otherwise = "All different"

main :: IO ()
main = print $ compareThree 3 3 4

-- MARATHON 6: BMI Calculator (with where)

module Marathon6 where
bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Overweight"
  | otherwise   = "Obese"
  where bmi = weight / height ^ 2

main :: IO ()
main = print $ bmiTell 70 1.75

-- MARATHON 7: BMI Calculator (with let)

module Marathon7 where
bmiTell :: Double -> Double -> String
bmiTell weight height =
  let bmi = weight / height ^ 2
  in if bmi <= 18.5 then "Underweight"
     else if bmi <= 25.0 then "Normal"
     else if bmi <= 30.0 then "Overweight"
     else "Obese"

main :: IO ()
main = print $ bmiTell 85 1.75

-- MARATHON 8: Nested where

module Marathon8 where
cylinderVolume :: Double -> Double -> Double
cylinderVolume r h = baseArea * h
  where baseArea = pi * r^2

main :: IO ()
main = print $ cylinderVolume 3 10

-- MARATHON 9: Tax Calculator (where)

module Marathon9 where
calcTax :: Double -> Double
calcTax income
  | income <= 10000 = income * low
  | income <= 50000 = income * mid
  | otherwise       = income * high
  where
    low = 0.05
    mid = 0.15
    high = 0.25

main :: IO ()
main = print $ calcTax 12000

-- MARATHON 10: Tax Calculator (let)

module Marathon10 where
calcTax :: Double -> Double
calcTax income =
  let low = 0.05
      mid = 0.15
      high = 0.25
  in if income <= 10000 then income * low
     else if income <= 50000 then income * mid
     else income * high

main :: IO ()
main = print $ calcTax 60000

-- MARATHON 11: Max of Three (guards)

module Marathon11 where
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c
  | a >= b && a >= c = a
  | b >= a && b >= c = b
  | otherwise        = c

main :: IO ()
main = print $ maxOfThree 5 12 9

-- MARATHON 12: Name Formatter

module Marathon12 where
formatName :: String -> String -> String
formatName first last = cap first ++ " " ++ cap last
  where
    cap (x:xs) = toUpper x : map toLower xs
    cap [] = []
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

main :: IO ()
main = print $ formatName "jOHn" "DOE"

-- MARATHON 13: Min of Two (guards)

module Marathon13 where
minOfTwo :: Int -> Int -> Int
minOfTwo x y
  | x < y     = x
  | otherwise = y

main :: IO ()
main = print $ minOfTwo 4 7

-- MARATHON 14: Leap Year Checker

module Marathon14 where
isLeapYear :: Int -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)

main :: IO ()
main = print $ isLeapYear 2024

-- MARATHON 15: Number Sign

module Marathon15 where
numberSign :: Int -> String
numberSign n
  | n > 0     = "Positive"
  | n < 0     = "Negative"
  | otherwise = "Zero"

main :: IO ()
main = print $ numberSign (-5)

-- MARATHON 16: Safe Division with Maybe

module Marathon16 where
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

main :: IO ()
main = print $ safeDiv 10 0

-- MARATHON 17: Voting Eligibility

module Marathon17 where
canVote :: Int -> String
canVote age = if age >= 18 then "Eligible" else "Not eligible"

main :: IO ()
main = print $ canVote 17

-- MARATHON 18: Circle Area using let

module Marathon18 where
circleArea :: Double -> Double
circleArea r =
  let piVal = 3.14159
  in piVal * r * r

main :: IO ()
main = print $ circleArea 5

-- MARATHON 19: Fibonacci with pattern match

module Marathon19 where
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = print $ fib 10

-- MARATHON 20: List Length (recursion)

module Marathon20 where
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

main :: IO ()
main = print $ listLength [1..10]
