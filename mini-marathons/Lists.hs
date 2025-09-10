
--order List Des
--remove Item List
--find Pos Item List

import Data.List (sortBy)
import Data.Ord (comparing)

orderListDec :: Ord a => [a] -> [a]
orderListDec x = sortBy (flip compare) x

removeItemList :: Eq a => a -> [a] -> [a]
removeItemList x = filter (\y -> y /= x)

main :: IO ()
main = do
 print $ orderListDec [8,10,7,5]
 print $ removeItemList 3 [2,3,5,6,7]
