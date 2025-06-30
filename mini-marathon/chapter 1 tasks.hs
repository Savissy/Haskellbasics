-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

mainHC1T1 :: IO ()
mainHC1T1 = print $ doubleThenIncrement 5  -- Expected: 11


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

mainHC1T2 :: IO ()
mainHC1T2 = print $ circleArea 3  -- Expected: ~28.27


-- HC1T3 - Task 3: Checking if a Number is Greater than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

mainHC1T3 :: IO ()
mainHC1T3 = print $ greaterThan18 20  -- Expected: True


-- HC1T4 - Task 4: Composing a Function to Process Player Data
extractPlayers :: [(String, Int)] -> [String]
extractPlayers = map fst

sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [(String, Int)] -> [(String, Int)]
topThree = take 3

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

mainHC1T4 :: IO ()
mainHC1T4 = print $ getTopThreePlayers [("Alice", 10), ("Bob", 30), ("Cara", 20), ("Dan", 40)]
-- Expected: ["Dan","Bob","Cara"]


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

mainHC1T5 :: IO ()
mainHC1T5 = print $ take 10 infiniteNumbers  -- Expected: [1..10]


-- HC1T6 - Task 6: Using Type Signatures
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

mainHC1T6 :: IO ()
mainHC1T6 = print $ addNumbers 3 4  -- Expected: 7


-- HC1T7 - Task 7: Converting Fahrenheit to Celsius
fToC :: Float -> Float
fToC f = (f - 32) * 5 / 9

mainHC1T7 :: IO ()
mainHC1T7 = print $ fToC 98.6  -- Expected: ~37.0


-- HC1T8 - Task 8: Higher-Order Functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

mainHC1T8 :: IO ()
mainHC1T8 = print $ applyTwice (+1) 3  -- Expected: 5


-- Optional: Run all main functions together
main :: IO ()
main = do
  putStrLn "HC1T1:"
  mainHC1T1
  putStrLn "HC1T2:"
  mainHC1T2
  putStrLn "HC1T3:"
  mainHC1T3
  putStrLn "HC1T4:"
  mainHC1T4
  putStrLn "HC1T5:"
  mainHC1T5
  putStrLn "HC1T6:"
  mainHC1T6
  putStrLn "HC1T7:"
  mainHC1T7
  putStrLn "HC1T8:"
  mainHC1T8
