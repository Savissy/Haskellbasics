data Pair a = Pair a a deriving (Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

main :: IO ()
main = do
    print $ Pair (+1) (*2) <*> Pair 10 20
    -- Expected: Pair 11 40

addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative a b c = (\x y z -> x + y + z) <$> a <*> b <*> c

main :: IO ()
main = do
    print $ addThreeApplicative (Just 1) (Just 2) (Just 3)   -- Just 6
    print $ addThreeApplicative (Just 1) Nothing (Just 3)    -- Nothing

safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequence

main :: IO ()
main = do
    print $ safeProduct [Just 2, Just 3, Just 4]   -- Just 24
    print $ safeProduct [Just 2, Nothing, Just 4] -- Nothing

import Control.Applicative (liftA2)

liftAndMultiply :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2

main :: IO ()
main = do
    print $ liftAndMultiply (*) (Just 2) (Just 3) -- Just 6
    print $ liftAndMultiply (+) (Just 4) Nothing -- Nothing

applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (io1, io2) = (+) <$> io1 <*> io2

main :: IO ()
main = do
    result <- applyEffects (print 5 >> return 5, print 7 >> return 7)
    putStrLn $ "Sum is: " ++ show result

import Control.Monad (forever)

repeatEffect :: IO () -> IO ()
repeatEffect action = forever action

main :: IO ()
main = do
    repeatEffect (putStrLn "Looping forever...") -- Press Ctrl+C to stop

import Control.Monad (when)

conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

main :: IO ()
main = do
    conditionalPrint True "This will print"
    conditionalPrint False "This will NOT print"
