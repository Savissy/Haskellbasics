module CoxyHasListExtras where

-- 🔢 Counting & Summing
countItemList :: Eq a => a -> [a] -> Int
countItemList _ [] = 0
countItemList y (x:xs)
  | x == y    = 1 + countItemList y xs
  | otherwise = countItemList y xs

sumList :: Num a => [a] -> a
sumList []     = 0
sumList (x:xs) = x + sumList xs

productList :: Num a => [a] -> a
productList []     = 1
productList (x:xs) = x * productList xs

-- 🧮 Averages
averageList :: (Fractional a) => [a] -> Maybe a
averageList [] = Nothing
averageList xs = Just (sumList xs / fromIntegral (length xs))

-- 🔄 Zipping & Pairing
zipLists :: [a] -> [b] -> [(a, b)]
zipLists [] _          = []
zipLists _ []          = []
zipLists (x:xs) (y:ys) = (x, y) : zipLists xs ys

-- 🎲 Splitting
splitAtList :: Int -> [a] -> ([a], [a])
splitAtList _ []     = ([], [])
splitAtList 0 xs     = ([], xs)
splitAtList n (x:xs) =
  let (first, rest) = splitAtList (n-1) xs
  in (x:first, rest)

-- 🪢 Flattening (lists of lists)
flattenList :: [[a]] -> [a]
flattenList []       = []
flattenList (xs:xss) = xs ++ flattenList xss

-- 🔀 Uniqueness & Sets
uniqueList :: Eq a => [a] -> [a]
uniqueList [] = []
uniqueList (x:xs)
  | elemList x xs = uniqueList xs
  | otherwise     = x : uniqueList xs

-- helper: your own elem check
elemList :: Eq a => a -> [a] -> Bool
elemList _ [] = False
elemList y (x:xs)
  | y == x    = True
  | otherwise = elemList y xs

-- 🧩 Grouping
groupList :: Eq a => [a] -> [[a]]
groupList [] = []
groupList (x:xs) =
  let (same, rest) = spanList (== x) xs
  in (x:same) : groupList rest

-- helper: custom span
spanList :: (a -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([], [])
spanList p (x:xs)
  | p x       = let (ys, zs) = spanList p xs in (x:ys, zs)
  | otherwise = ([], x:xs)
