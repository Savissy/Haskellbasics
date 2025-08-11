-- Define the Severity data type
data Severity = Low | Medium | High | Critical
    deriving (Show, Eq, Ord)

-- Implement the Semigroup instance
instance Semigroup Severity where
    (<>) s1 s2 = max s1 s2  -- Higher severity wins

-- Example main function for testing
main :: IO ()
main = do
    let a = Low
    let b = High
    let c = Critical
    let d = Medium

    putStrLn $ "Low <> High = " ++ show (a <> b)        -- High
    putStrLn $ "Medium <> High = " ++ show (d <> b)     -- High
    putStrLn $ "Critical <> Medium = " ++ show (c <> d) -- Critical
    putStrLn $ "Low <> Medium = " ++ show (a <> d)      -- Medium

newtype Min a = Min { getMin :: a }
  deriving (Show, Eq)

newtype Max a = Max { getMax :: a }
  deriving (Show, Eq)

instance Ord a => Semigroup (Min a) where
  (Min x) <> (Min y) = Min (min x y)

instance Ord a => Semigroup (Max a) where
  (Max x) <> (Max y) = Max (max x y)

main :: IO ()
main = do
  print $ Min 5 <> Min 3       -- Min {getMin = 3}
  print $ Max 5 <> Max 3       -- Max {getMax = 5}

data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

instance Semigroup Severity where
  x <> y = max x y

instance Monoid Severity where
  mempty = Low

main :: IO ()
main = do
  print $ Low <> High          -- High
  print $ mconcat [Low, Medium, Critical]  -- Critical
  

newtype Sum a = Sum { getSum :: a }
  deriving (Show, Eq)

instance Num a => Semigroup (Sum a) where
  (Sum x) <> (Sum y) = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

main :: IO ()
main = do
  print $ Sum 5 <> Sum 10      -- Sum {getSum = 15}
  print $ mconcat [Sum 1, Sum 2, Sum 3]  -- Sum {getSum = 6}
  

data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

instance Semigroup Severity where
  x <> y = max x y

instance Monoid Severity where
  mempty = Low

maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

main :: IO ()
main = do
  print $ maxSeverity [Low, Medium, High]        -- High
  print $ maxSeverity [Low, Low, Medium]         -- Medium

newtype Product a = Product { getProduct :: a }
  deriving (Show, Eq)

instance Num a => Semigroup (Product a) where
  (Product x) <> (Product y) = Product (x * y)

instance Num a => Monoid (Product a) where
  mempty = Product 1

multiplyProducts :: Num a => [Product a] -> Product a
multiplyProducts = mconcat

main :: IO ()
main = do
  print $ multiplyProducts [Product 2, Product 3, Product 4]  -- Product {getProduct = 24}

  
