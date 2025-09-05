import Data.Time

type Dealer       = String
type Crop         = String
type Quantity     = Double
type Price        = Double
type IsRipe       = Bool

data ProductDetails = ProductDetails 
  {cName  :: Crop
  ,qty    :: Quantity
  ,price  :: Price
  ,total  :: Double
  ,isRipe :: IsRipe
  ,date   :: Day
  } deriving (Show)

data ProductList = ProductList {list :: [ProductDetails]} deriving (Show)
data Manufacturer = Manufacturer {dealer :: Dealer, prod :: [ProductList]} deriving (Show)

instance Semigroup ProductList where
  (<>) (ProductList a) (ProductList b) = ProductList (a <> b)
instance Monoid ProductList where
  mempty = ProductList []
  mappend (ProductList a) (ProductList b) = ProductList (a <> b)

instance Semigroup Manufacturer where
  (<>) (Manufacturer a b) (Manufacturer c d) = Manufacturer (a <> " , " <> c) (b <> d)
instance Monoid Manufacturer where
  mempty = Manufacturer "" []
  mappend (Manufacturer a b) (Manufacturer c d) = Manufacturer (a <> " , " <> c) (b <> d)

instance Semigroup ProductDetails where
  (<>) (ProductDetails x y z k l m ) (ProductDetails n o p q r s ) = 
    ProductDetails 
    (x <> ", " <> n)
    (max y o)
    (max z p)
    (max k q)
    (l || r)
    (max m s)
instance Monoid ProductDetails where
  mempty = ProductDetails
    {cName  = ""
    ,qty    = 0
    ,price  = 0
    ,total  = 0
    ,isRipe = False
    ,date   = ModifiedJulianDay 0 
    } 
  mappend (ProductDetails x y z k l m ) (ProductDetails n o p q r s ) =
    ProductDetails 
    (x <> ", " <> n)
    (max y o)
    (max z p)
    (max k q)
    (l || r)
    (max m s)
  
addDetails :: Crop -> Quantity -> Price -> IsRipe -> IO ProductDetails
addDetails x y z p = do
  currentTime <- getCurrentTime
  let today = utctDay currentTime
  let totalPrice =( y * z ) :: Double
  return $ 
    ProductDetails {cName = x, qty = y, price = z,total = totalPrice, isRipe = p, date = today}

addProduct :: ProductDetails -> ProductList
addProduct x = ProductList [x]

addNewProduct :: ProductDetails -> ProductList -> ProductList
addNewProduct x (ProductList xs) = ProductList (x: xs)

finalOutput :: Dealer -> ProductList -> Manufacturer
finalOutput x y = Manufacturer { dealer = x , prod = [y]}

 
main :: IO ()
main = do
    putStrLn "Input first product details (cropname):"
    crop1 <- getLine
    putStrLn "Quantity:"
    qty1 <- readLn :: IO Double
    putStrLn "Price:"
    price1 <- readLn :: IO Double
    putStrLn "Is ripe (True/False):"
    ripe1Str <- getLine
    let ripe1 = case ripe1Str of
          "True"  -> True
          "true"  -> True
          _       -> False

    putStrLn "Input second product details (cropname):"
    crop2 <- getLine
    putStrLn "Quantity:"
    qty2 <- readLn :: IO Double
    putStrLn "Price:"
    price2 <- readLn :: IO Double
    putStrLn "Is ripe (True/False):"
    ripe2Str <- getLine
    let ripe2 = case ripe2Str of
          "True"  -> True
          "true"  -> True
          _       -> False

    putStrLn "Input third product details (cropname):"
    crop3 <- getLine
    putStrLn "Quantity:"
    qty3 <- readLn :: IO Double
    putStrLn "Price:"
    price3 <- readLn :: IO Double
    putStrLn "Is ripe (True/False):"
    ripe3Str <- getLine
    let ripe3 = case ripe3Str of
          "True"  -> True
          "true"  -> True
          _       -> False

    firstProduct <- addDetails crop1 qty1 price1 ripe1
    secondProduct <- addDetails crop2 qty2 price2 ripe2
    thirdProduct <- addDetails crop3 qty3 price3 ripe3
  
    let list1 = addProduct thirdProduct
        list2 = addNewProduct secondProduct list1
        list3 = addNewProduct firstProduct list2
  --let nameMan = "Israel"
  --let newName = "Saviour"
  --let fullList = mempty <> mappend list1  list2
  
  --let fOutput = finalOutput nameMan fullList
  --let fOutput2 = finalOutput newName list2
  
  --print $ mempty <> mappend fOutput fOutput2
  --print $ mempty <> mappend firstProduct secondProduct <> thirdProduct

    print list3
  
