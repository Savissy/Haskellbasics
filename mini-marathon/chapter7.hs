-- HC7T1
data Color = Red | Green | Blue deriving Show

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

main1 :: IO ()
main1 = do
  print (Red == Red)      -- True
  print (Red == Blue)     -- False

-- HC7T2
instance Ord Color where
  compare Red Green  = LT
  compare Red Blue   = LT
  compare Green Blue = LT
  compare a b
    | a == b    = EQ
    | otherwise = GT

main2 :: IO ()
main2 = do
  print (Red < Green)     -- True
  print (Green < Blue)    -- True
  print (Blue > Red)      -- True

-- HC7T3
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

main3 :: IO ()
main3 = do
  print $ compareValues 5 3       -- 5
  print $ compareValues "a" "z"   -- "z"

-- HC7T4
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
  show (Circle r) = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
  readsPrec _ input =
    case words input of
      ["Circle", r] ->
        [(Circle (read r), "")]
      ["Rectangle", w, h] ->
        [(Rectangle (read w) (read h), "")]
      _ -> []

main4 :: IO ()
main4 = do
  print $ show (Circle 5.0)
  print $ read "Rectangle 3.0 4.0" :: Shape

-- HC7T5
squareArea :: Num a => a -> a
squareArea side = side * side

main5 :: IO ()
main5 = do
  print $ squareArea 5       -- 25
  print $ squareArea 3.5     -- 12.25

-- HC7T6
circleCircumference :: (Floating a, Integral b) => b -> a
circleCircumference r = 2 * pi * fromIntegral r

main6 :: IO ()
main6 = do
  print $ circleCircumference 5     -- 31.41592653589793

-- HC7T7
instance Enum Color where
  fromEnum Red = 0
  fromEnum Green = 1
  fromEnum Blue = 2

  toEnum 0 = Red
  toEnum 1 = Green
  toEnum 2 = Blue
  toEnum _ = error "Invalid enum"

instance Bounded Color where
  minBound = Red
  maxBound = Blue

nextColor :: Color -> Color
nextColor c = if c == maxBound then minBound else succ c

main7 :: IO ()
main7 = do
  print $ nextColor Red    -- Green
  print $ nextColor Blue   -- Red (wrap around)

-- HC7T8
parseShape :: String -> Maybe Shape
parseShape str =
  case reads str of
    [(s, "")] -> Just s
    _         -> Nothing

main8 :: IO ()
main8 = do
  print $ parseShape "Circle 5.0"             -- Just (Circle 5.0)
  print $ parseShape "Rectangle 3.0 4.0"      -- Just (Rectangle 3.0 4.0)
  print $ parseShape "Triangle 2.0 3.0"       -- Nothing

-- HC7T9
class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True  = "Yes"
  describe False = "No"

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle of width " ++ show w ++ " and height " ++ show h

main9 :: IO ()
main9 = do
  print $ describe True
  print $ describe (Circle 4.0)

-- HC7T10
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (max x y)

-- Make Shape an instance of Ord (e.g., compare by area)
instance Ord Shape where
  compare (Circle r1) (Circle r2) = compare r1 r2
  compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1 * h1) (w2 * h2)
  compare (Circle r) (Rectangle w h) = compare (pi * r * r) (w * h)
  compare (Rectangle w h) (Circle r) = compare (w * h) (pi * r * r)

main10 :: IO ()
main10 = do
  let s1 = Circle 3.0
      s2 = Rectangle 3.0 5.0
  putStrLn $ describeAndCompare s1 s2
