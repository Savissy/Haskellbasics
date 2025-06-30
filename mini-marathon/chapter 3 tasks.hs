-- Haskell Function Composition & Improvement Marathons

-- MARATHON 1: Double then Increment

module FuncMarathon1 where
doubleInc :: Int -> Int
doubleInc = (+1) . (*2)

main :: IO ()
main = print $ doubleInc 5

-- MARATHON 2: Length of Uppercase

module FuncMarathon2 where
import Data.Char (toUpper)

lenUpper :: String -> Int
lenUpper = length . map toUpper

main :: IO ()
main = print $ lenUpper "hello"

-- MARATHON 3: Square then Show

module FuncMarathon3 where
squareShow :: Int -> String
squareShow = show . (^2)

main :: IO ()
main = print $ squareShow 4

-- MARATHON 4: Filter Evens and Double

module FuncMarathon4 where
process :: [Int] -> [Int]
process = map (*2) . filter even

main :: IO ()
main = print $ process [1..10]

-- MARATHON 5: Compose with Custom Function

module FuncMarathon5 where
addThree :: Int -> Int
addThree = (+3)

timesFour :: Int -> Int
timesFour = (*4)

combo :: Int -> Int
combo = timesFour . addThree

main :: IO ()
main = print $ combo 2

-- MARATHON 6: Absolute Difference

module FuncMarathon6 where
absDiff :: Int -> Int -> Int
absDiff = (abs .) . (-)

main :: IO ()
main = print $ absDiff 3 8

-- MARATHON 7: Compose on Maybe

module FuncMarathon7 where
safeSqrt :: Floating a => a -> Maybe a
safeSqrt x = if x >= 0 then Just (sqrt x) else Nothing

safeHalf :: Fractional a => a -> Maybe a
safeHalf x = Just (x / 2)

combine :: Double -> Maybe Double
combine = safeSqrt <=< safeHalf -- requires Control.Monad

main :: IO ()
main = print $ combine 16

-- MARATHON 8: Clean and Split

module FuncMarathon8 where
cleanSplit :: String -> [String]
cleanSplit = words . map sanitize
  where sanitize c = if c `elem` ",.;" then ' ' else c

main :: IO ()
main = print $ cleanSplit "Hello, world. How are you?"

-- MARATHON 9: Sum of Squares

module FuncMarathon9 where
sumSquares :: [Int] -> Int
sumSquares = sum . map (^2)

main :: IO ()
main = print $ sumSquares [1..5]

-- MARATHON 10: Compose with foldr

module FuncMarathon10 where
composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id

main :: IO ()
main = print $ composeAll [(+1), (*2), subtract 3] 4

-- MARATHON 11: Format Number List

module FuncMarathon11 where
formatList :: [Int] -> String
formatList = unwords . map show

main :: IO ()
main = print $ formatList [1, 2, 3, 4]

-- MARATHON 12: Extract and Capitalize

module FuncMarathon12 where
import Data.Char (toUpper)

capitalizeWords :: String -> [String]
capitalizeWords = map (map toUpper) . words

main :: IO ()
main = print $ capitalizeWords "welcome to haskell"

-- MARATHON 13: Even Sum

module FuncMarathon13 where
evenSum :: [Int] -> Int
evenSum = sum . filter even

main :: IO ()
main = print $ evenSum [1..10]

-- MARATHON 14: Function Inversion

module FuncMarathon14 where
negateAbs :: Int -> Int
negateAbs = negate . abs

main :: IO ()
main = print $ negateAbs (-10)

-- MARATHON 15: Chain With Maybe

module FuncMarathon15 where
import Control.Monad ((>=>))

safeSqrt :: Double -> Maybe Double
safeSqrt x = if x >= 0 then Just (sqrt x) else Nothing

safeRecip :: Double -> Maybe Double
safeRecip x = if x /= 0 then Just (1 / x) else Nothing

safeChain :: Double -> Maybe Double
safeChain = safeSqrt >=> safeRecip

main :: IO ()
main = print $ safeChain 4

-- MARATHON 16: Compose with Flip

module FuncMarathon16 where
customSubtract :: Int -> Int -> Int
customSubtract = flip (-)

main :: IO ()
main = print $ customSubtract 5 10

-- MARATHON 17: Nested Function Calls

module FuncMarathon17 where
result :: Int
result = ceiling . sqrt . fromIntegral $ 50

main :: IO ()
main = print result

-- MARATHON 18: Combine Boolean Checks

module FuncMarathon18 where
isEvenPositive :: Int -> Bool
isEvenPositive = (> 0) . abs

main :: IO ()
main = print $ isEvenPositive (-4)

-- MARATHON 19: List Reverse and Show

module FuncMarathon19 where
reverseShow :: Show a => [a] -> String
reverseShow = show . reverse

main :: IO ()
main = print $ reverseShow [1..5]

-- MARATHON 20: Apply Twice

module FuncMarathon20 where
applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

main :: IO ()
main = print $ applyTwice (+3) 4
