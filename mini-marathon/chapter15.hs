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



    


