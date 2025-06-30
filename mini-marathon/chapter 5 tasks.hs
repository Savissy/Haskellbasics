-- Haskell Pattern Matching Marathons (Independent Set)

-- MARATHON 1: Match Int with Cases

module PatternMarathon1 where
matchInt :: Int -> String
matchInt 0 = "Zero"
matchInt 1 = "One"
matchInt _ = "Other"

main :: IO ()
main = print $ matchInt 1

-- MARATHON 2: Match Maybe String

module PatternMarathon2 where
matchMaybeStr :: Maybe String -> String
matchMaybeStr (Just s) = "Found: " ++ s
matchMaybeStr Nothing = "Nothing there"

main :: IO ()
main = print $ matchMaybeStr (Just "hi")

-- MARATHON 3: Match Triple

module PatternMarathon3 where
matchTriple :: (Int, Int, Int) -> Int
matchTriple (x, y, z) = x + y + z

main :: IO ()
main = print $ matchTriple (1,2,3)

-- MARATHON 4: Match Head and Tail

module PatternMarathon4 where
matchHeadTail :: [Int] -> String
matchHeadTail [] = "Empty"
matchHeadTail (x:xs) = "Head is " ++ show x

main :: IO ()
main = print $ matchHeadTail [5,6,7]

-- MARATHON 5: Match Custom Type

module PatternMarathon5 where
data TrafficLight = Red | Yellow | Green

lightAction :: TrafficLight -> String
lightAction Red = "Stop"
lightAction Yellow = "Caution"
lightAction Green = "Go"

main :: IO ()
main = print $ lightAction Yellow

-- MARATHON 6: Match Either Bool String

module PatternMarathon6 where
handleResult :: Either Bool String -> String
handleResult (Left True) = "Left True"
handleResult (Left False) = "Left False"
handleResult (Right msg) = "Message: " ++ msg

main :: IO ()
main = print $ handleResult (Right "done")

-- MARATHON 7: Match List by Length

module PatternMarathon7 where
lengthType :: [a] -> String
lengthType [] = "Empty"
lengthType [x] = "Single"
lengthType [x, y] = "Pair"
lengthType _ = "Longer"

main :: IO ()
main = print $ lengthType [1,2,3]

-- MARATHON 8: Match Nested Pattern

module PatternMarathon8 where
nestedMatch :: [(Int, Int)] -> String
nestedMatch ((1,1):_) = "Found (1,1)"
nestedMatch _ = "Not found"

main :: IO ()
main = print $ nestedMatch [(1,1),(2,2)]

-- MARATHON 9: Match With Guards

module PatternMarathon9 where
signOf :: Int -> String
signOf n
  | n < 0 = "Negative"
  | n == 0 = "Zero"
  | otherwise = "Positive"

main :: IO ()
main = print $ signOf (-7)

-- MARATHON 10: Match in Let Binding

module PatternMarathon10 where
main :: IO ()
main =
  let (a, b) = (3, 4)
  in print $ a + b

-- MARATHON 11: As-pattern

module PatternMarathon11 where
showFullList :: [Char] -> String
showFullList s@(x:_) = "First: " ++ [x] ++ ", Full: " ++ s
showFullList [] = "Empty"

main :: IO ()
main = print $ showFullList "hello"

-- MARATHON 12: Match List Using Case

module PatternMarathon12 where
checkList :: [Int] -> String
checkList xs = case xs of
  [] -> "Empty"
  [x] -> "Single: " ++ show x
  _ -> "Multiple"

main :: IO ()
main = print $ checkList [99]

-- MARATHON 13: Match Bool with Case

module PatternMarathon13 where
boolCase :: Bool -> String
boolCase b = case b of
  True -> "Yes"
  False -> "No"

main :: IO ()
main = print $ boolCase True

-- MARATHON 14: Match Enum Type

module PatternMarathon14 where
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

main :: IO ()
main = print $ isWeekend Fri

-- MARATHON 15: Match Multiple Constructors

module PatternMarathon15 where
data Result = Ok Int | Error String

handleResult :: Result -> String
handleResult (Ok n) = "Value: " ++ show n
handleResult (Error msg) = "Error: " ++ msg

main :: IO ()
main = print $ handleResult (Ok 42)

-- MARATHON 16: Match Partial Record

module PatternMarathon16 where
data Person = Person { name :: String, age :: Int }

getName :: Person -> String
getName (Person { name = n }) = n

main :: IO ()
main = print $ getName (Person "Alice" 30)

-- MARATHON 17: Match List with Pattern Guard

module PatternMarathon17 where
startsWithOne :: [Int] -> Bool
startsWithOne (1:_) = True
startsWithOne _ = False

main :: IO ()
main = print $ startsWithOne [1,2,3]

-- MARATHON 18: Match Empty and Nonempty Tuples

module PatternMarathon18 where
tupleType :: (Int, Int) -> String
tupleType (0, 0) = "Both zero"
tupleType (x, y) = "Something else"

main :: IO ()
main = print $ tupleType (0,0)

-- MARATHON 19: Match Custom Recursive Type

module PatternMarathon19 where
data List a = Empty | Cons a (List a)

listLength :: List a -> Int
listLength Empty = 0
listLength (Cons _ xs) = 1 + listLength xs

main :: IO ()
main = print $ listLength (Cons 1 (Cons 2 Empty))

-- MARATHON 20: Match With Wildcard

module PatternMarathon20 where
ignoreSecond :: (Int, Int) -> Int
ignoreSecond (x, _) = x

main :: IO ()
main = print $ ignoreSecond (5, 99)
