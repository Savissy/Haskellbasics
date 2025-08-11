import System.IO
import Control.Exception
import Text.Read (readMaybe)

-- Safe function to read a number from the user
getNumber :: Read a => String -> IO a
getNumber prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    case readMaybe input of
        Just val -> return val
        Nothing  -> do
            putStrLn "Invalid input. Please enter a number."
            getNumber prompt

-- Main program
main :: IO ()
main = do
    -- Handle file reading exceptions
    fileContent <- catch (readFile "distance.txt")
                         (\e -> do
                             putStrLn $ "Error reading file: " ++ show (e :: IOException)
                             return "0")

    let distance = case readMaybe fileContent of
                      Just d  -> d
                      Nothing -> 0  -- If file content isn't a number

    time <- getNumber "Enter time (in seconds): "

    if time == 0
       then putStrLn "Time cannot be zero."
       else putStrLn $ "Velocity = " ++ show (distance / time) ++ " m/s"

import Control.Exception
import Data.Char (toUpper)

-- Define our own error type for unknown colors
data TrafficLightError = UnknownColor String
  deriving Show

instance Exception TrafficLightError

-- Car reaction function
reactToLight :: String -> IO ()
reactToLight color =
    case map toUpper color of
        "GREEN"  -> putStrLn "Car is accelerating..."
        "YELLOW" -> putStrLn "Car is preparing to stop..."
        "RED"    -> putStrLn "Car is stopping."
        _        -> throwIO (UnknownColor color)

-- Main program with error handling
main :: IO ()
main = do
    putStrLn "Enter traffic light color (GREEN, YELLOW, RED):"
    color <- getLine

    -- Try to react and catch any errors
    catch (reactToLight color)
          (\(UnknownColor c) -> putStrLn $ "Error: Unknown color '" ++ c ++ "'. Car will stop for safety.")

-- Safe division function
safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing          -- If denominator is zero, return Nothing
safeDiv x y = Just (x / y)     -- Otherwise, return the result wrapped in Just

main :: IO ()
main = do
    print (safeDiv 10 2)   -- Just 5.0
    print (safeDiv 7 0)    -- Nothing

import Text.Read (readMaybe)

-- Safe division
safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

main :: IO ()
main = do
    putStrLn "Enter the numerator:"
    numStr <- getLine

    putStrLn "Enter the denominator:"
    denStr <- getLine

    case (readMaybe numStr :: Maybe Double, readMaybe denStr :: Maybe Double) of
        (Just num, Just den) ->
            case safeDiv num den of
                Just result -> putStrLn $ "Result: " ++ show result
                Nothing     -> putStrLn "Error: Division by zero."
        _ -> putStrLn "Invalid input! Please enter valid numbers."

import Text.Read (readMaybe)

-- Safe division using Maybe
safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

-- Calculate velocity safely
calculateVelocity :: Double -> Double -> Maybe Double
calculateVelocity distance time = safeDiv distance time

main :: IO ()
main = do
    putStrLn "Enter distance (in meters):"
    distStr <- getLine

    putStrLn "Enter time (in seconds):"
    timeStr <- getLine

    -- Try to parse both values safely
    let maybeDistance = readMaybe distStr :: Maybe Double
        maybeTime     = readMaybe timeStr :: Maybe Double

    case (maybeDistance, maybeTime) of
        (Just d, Just t) ->
            case calculateVelocity d t of
                Just v  -> putStrLn $ "Velocity = " ++ show v ++ " m/s"
                Nothing -> putStrLn "Error: Time cannot be zero."
        _ -> putStrLn "Invalid input: Please enter valid numbers."

-- Safe division with detailed error messages
safeDivEither :: (Eq a, Fractional a, Show a) => a -> a -> Either String a
safeDivEither _ 0 = Left "Error: Division by zero is not allowed."
safeDivEither x y = Right (x / y)

main :: IO ()
main = do
    print $ safeDivEither 10 2   -- Right 5.0
    print $ safeDivEither 7 0    -- Left "Error: Division by zero is not allowed."

    


