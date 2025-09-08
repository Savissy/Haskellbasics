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
  }
  |
  Nocrop
  {qty :: Quantity
  ,price  :: Price
  ,total  :: Double
  ,isRipe :: IsRipe
  ,date   :: Day
  }
  |
  Noqty
  {cName :: Crop
  ,price  :: Price
  ,isRipe :: IsRipe
  ,date   :: Day
  }
  |
  Noprice
  {cName  :: Crop
  ,qty    :: Quantity
  ,isRipe :: IsRipe
  ,date   :: Day
  }deriving (Show)

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

deleteProductList :: ProductList -> ProductList
deleteProductList (ProductList x) = ProductList []

deleteCrop :: ProductDetails -> ProductDetails
deleteCrop (ProductDetails y e f g h i) = Nocrop {qty = e, price = f, total = g, isRipe = h, date = i} 

deleteQuantity :: ProductDetails -> ProductDetails
deleteQuantity (ProductDetails y e f g h i) = Noqty {cName = y, price = f, isRipe = h, date = i}

deletePrice :: ProductDetails -> ProductDetails
deletePrice (ProductDetails y e f g h i) = Noprice {cName = y, qty = e, isRipe = h, date = i} 

editCrop :: Crop -> ProductDetails -> ProductDetails
editCrop x (ProductDetails y e f g h i) = ProductDetails {cName = x, qty = e, price = f, total = g, isRipe = h, date = i}

editQuantity :: Quantity -> ProductDetails -> ProductDetails
editQuantity x (ProductDetails y e f g h i) = ProductDetails {cName = y, qty = x, price = f, total = (f * x), isRipe = h, date = i}

editPrice :: Price -> ProductDetails -> ProductDetails
editPrice x (ProductDetails y e f g h i) = ProductDetails {cName = y, qty = e, price = x, total = (e * x), isRipe = h, date = i} 

addNewProduct :: ProductDetails -> ProductList -> ProductList
addNewProduct x (ProductList xs) = ProductList (x: xs)

finalOutput :: Dealer -> ProductList -> Manufacturer
finalOutput x y = Manufacturer { dealer = x , prod = [y]}

 
main :: IO ()
main = do
  firstProduct <- addDetails "Banana" 7 30 True 
  secondProduct <- addDetails "Oranges" 5 60 False 
  thirdProduct <- addDetails "Spinach" 8 30 True 
  
  let list1 = addProduct firstProduct
  let list2 = addNewProduct secondProduct list1
  let nameMan = "Israel"
  let newName = "Saviour"
  let fullList = mempty <> mappend list1  list2
  
  let fOutput = finalOutput nameMan fullList
  let fOutput2 = finalOutput newName list2
  
  print $ fullList
  print $ editCrop "Apple" firstProduct
  print $ editQuantity 8 firstProduct
  print $ editPrice 50 firstProduct
  print $ deleteCrop firstProduct
  print $ deleteQuantity firstProduct
  print $ deletePrice firstProduct
  print $ deleteProductList fullList
  print $ mempty <> mappend firstProduct secondProduct <> thirdProduct
  
