safeDivide :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

main :: IO ()
main = do
    print $ safeDivide 10 2    -- Just 5.0
    print $ safeDivide 10 0    -- Nothing

sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe [] = Just []
sequenceMaybe (Nothing:_) = Nothing
sequenceMaybe (Just x:xs) = do
    rest <- sequenceMaybe xs
    return (x : rest)

main :: IO ()
main = do
    print $ sequenceMaybe [Just 1, Just 2, Just 3] -- Just [1,2,3]
    print $ sequenceMaybe [Just 1, Nothing, Just 3] -- Nothing

import Control.Monad.Writer

type CalcLog = Writer [String] Int

add :: Int -> Int -> CalcLog
add x y = do
    tell ["Added " ++ show x ++ " and " ++ show y]
    return (x + y)

multiply :: Int -> Int -> CalcLog
multiply x y = do
    tell ["Multiplied " ++ show x ++ " and " ++ show y]
    return (x * y)

main :: IO ()
main = do
    let (result, log) = runWriter $ do
            a <- add 3 5
            b <- multiply a 10
            return b
    print result
    mapM_ putStrLn log

import Control.Monad.State
import Control.Monad (when)

countChars :: Char -> String -> State Int ()
countChars _ [] = return ()
countChars c (x:xs) = do
    when (x == c) $ modify (+1)
    countChars c xs

main :: IO ()
main = do
    let str = "hello world"
        target = 'l'
        count = execState (countChars target str) 0
    putStrLn $ "Count of '" ++ [target] ++ "': " ++ show count

import Control.Monad.Reader

data Config = Config { greeting :: String, username :: String }

greet :: Reader Config String
greet = do
    g <- asks greeting
    u <- asks username
    return (g ++ ", " ++ u ++ "!")

main :: IO ()
main = do
    let config = Config "Hello" "Alice"
    putStrLn $ runReader greet config

doubleMonad :: Maybe [a] -> Maybe [a]
doubleMonad mList = do
    xs <- mList
    return (xs ++ xs)

main :: IO ()
main = do
    print $ doubleMonad (Just [1, 2, 3]) -- Just [1,2,3,1,2,3]
    --print $ doubleMonad Nothing -- Nothing

findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "No matching element found"
findFirst p (x:xs)
    | p x       = Right x
    | otherwise = findFirst p xs

main :: IO ()
main = do
    print $ findFirst (> 5) [1, 2, 3, 10, 4]
    print $ findFirst (> 20) [1, 2, 3]

import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \inp -> [(f a, rest) | (a, rest) <- p inp]

instance Applicative Parser where
    pure a = Parser $ \inp -> [(a, inp)]
    (Parser pf) <*> (Parser pa) = Parser $ \inp ->
        [(f a, rest2) | (f, rest1) <- pf inp, (a, rest2) <- pa rest1]

instance Monad Parser where
    (Parser pa) >>= f = Parser $ \inp ->
        concat [runParser (f a) rest | (a, rest) <- pa inp]

instance Alternative Parser where
    empty = Parser $ const []
    (Parser p1) <|> (Parser p2) = Parser $ \inp ->
        p1 inp ++ p2 inp

charP :: Char -> Parser Char
charP c = Parser $ \inp -> case inp of
    (x:xs) | x == c -> [(c, xs)]
    _               -> []

digitP :: Parser Char
digitP = Parser $ \inp -> case inp of
    (x:xs) | isDigit x -> [(x, xs)]
    _                  -> []

numberP :: Parser Int
numberP = read <$> some digitP

addExpr :: Parser Int
addExpr = do
    n1 <- numberP
    _ <- charP '+'
    n2 <- numberP
    return (n1 + n2)

main :: IO ()
main = do
    print $ runParser addExpr "12+34"
    print $ runParser addExpr "5+xyz"
