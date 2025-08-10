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


    


