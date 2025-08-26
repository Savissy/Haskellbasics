{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LoggingSystem where

import Control.Applicative (Applicative(..))
import Control.Monad (ap, when)
import Data.List (intercalate)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (IOMode(..), withFile, hPutStrLn)
import System.FilePath ((</>))

-- LogEntry is a Semigroup and Monoid
data LogLevel = DEBUG | INFO | WARN | ERROR | CRITICAL
    deriving (Eq, Show, Ord, Enum, Bounded)

instance Semigroup LogLevel where
    a <> b = max a b  -- Combine by taking the more severe level

data LogEntry = LogEntry
    { level   :: LogLevel
    , time    :: String
    , message :: String
    , context :: [(String, String)]
    } deriving (Eq)

instance Show LogEntry where
    show (LogEntry lvl t msg ctx) = 
        "[" ++ show lvl ++ "] " ++ t ++ " " ++ msg ++ 
        (if null ctx then "" else " {" ++ formatContext ctx ++ "}")

formatContext :: [(String, String)] -> String
formatContext = intercalate ", " . map (\(k,v) -> k ++ "=" ++ v)

instance Semigroup LogEntry where
    (LogEntry l1 t1 m1 c1) <> (LogEntry l2 t2 m2 c2) =
        LogEntry (l1 <> l2) t1 (m1 ++ "; " ++ m2) (c1 ++ c2)

instance Monoid LogEntry where
    mempty = LogEntry DEBUG "" "" []

-- Logger is a Functor, Applicative, and Monad
newtype Logger a = Logger { runLogger :: (a, [LogEntry]) }

instance Functor Logger where
    fmap f (Logger (a, logs)) = Logger (f a, logs)

instance Applicative Logger where
    pure x = Logger (x, [])
    (Logger (f, logs1)) <*> (Logger (x, logs2)) = 
        Logger (f x, logs1 ++ logs2)

instance Monad Logger where
    return = pure
    (Logger (x, logs1)) >>= f =
        let Logger (y, logs2) = f x
        in Logger (y, logs1 ++ logs2)

-- Helper functions for logging
logMessage :: LogLevel -> String -> [(String, String)] -> Logger ()
logMessage lvl msg ctx = do
    timestamp <- Logger (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime, [])
    let entry = LogEntry lvl timestamp msg ctx
    Logger ((), [entry])

debug, info, warn, error, critical :: String -> [(String, String)] -> Logger ()
debug    msg ctx = logMessage DEBUG    msg ctx
info     msg ctx = logMessage INFO     msg ctx
warn     msg ctx = logMessage WARN     msg ctx
error    msg ctx = logMessage ERROR    msg ctx
critical msg ctx = logMessage CRITICAL msg ctx

-- File handling with resource management
data LogConfig = LogConfig
    { logDirectory :: FilePath
    , logFileName  :: String
    , minLogLevel  :: LogLevel
    }

defaultConfig :: LogConfig
defaultConfig = LogConfig "logs" "app.log" INFO

-- Write logs to file with proper error handling
writeLogsToFile :: LogConfig -> [LogEntry] -> IO ()
writeLogsToFile config entries = do
    createDirectoryIfMissing True (logDirectory config)
    let filePath = logDirectory config </> logFileName config
    -- Filter entries based on minimum log level
    let filteredEntries = filter (\e -> level e >= minLogLevel config) entries
    
    withFile filePath AppendMode $ \handle -> do
        fileExists <- doesFileExist filePath
        when (not fileExists) $ do
            hPutStrLn handle "=== APPLICATION LOG ==="
        
        mapM_ (hPutStrLn handle . show) filteredEntries

-- Example application using our logging system
processPayment :: Double -> String -> Logger Bool
processPayment amount userId = do
    info "Processing payment" [("amount", show amount), ("user", userId)]
    
    -- Validate amount
    when (amount <= 0) $ do
        error "Invalid payment amount" [("amount", show amount)]
        return ()
    
    -- Simulate payment processing
    if amount > 1000
        then do
            warn "Large payment processed" [("amount", show amount)]
            return True
        else do
            info "Payment processed successfully" [("amount", show amount)]
            return True

validateUser :: String -> Logger Bool
validateUser userId = do
    info "Validating user" [("user", userId)]
    
    -- Simulate validation
    if userId `elem` ["user1", "user2", "user3"]
        then do
            info "User validated" [("user", userId)]
            return True
        else do
            error "User validation failed" [("user", userId)]
            return False

-- Combined operation using monadic composition
processUserPayment :: String -> Double -> Logger Bool
processUserPayment userId amount = do
    isValid <- validateUser userId
    if isValid
        then processPayment amount userId
        else do
            error "Payment failed: invalid user" [("user", userId)]
            return False

-- Example of applicative style usage
createSummaryReport :: Logger (String, Double)
createSummaryReport = do
    payment1 <- processPayment 100 "user1"
    payment2 <- processPayment 250 "user2"
    
    -- Using applicative to combine results
    let summary = (,) <$> pure "Total processed" <*> pure (100 + 250)
    
    info "Summary report generated" []
    return summary

-- Run the logger and write to file
runApp :: IO ()
runApp = do
    let config = defaultConfig { minLogLevel = DEBUG }
    
    -- Run our payment processing example
    let (result, logs) = runLogger $ processUserPayment "user1" 150.0
    
    putStrLn $ "Payment result: " ++ show result
    writeLogsToFile config logs
    
    -- Run another example with multiple operations
    let (summary, summaryLogs) = runLogger createSummaryReport
    putStrLn $ "Summary: " ++ show summary
    writeLogsToFile config summaryLogs

-- Utility function to extract just the logs
getLogs :: Logger a -> [LogEntry]
getLogs = snd . runLogger

-- Example of using Semigroup and Monoid with logs
combineLogs :: Logger a -> Logger b -> Logger ()
combineLogs logger1 logger2 = do
    let (_, logs1) = runLogger logger1
    let (_, logs2) = runLogger logger2
    Logger ((), logs1 <> logs2)  -- Using the Semigroup instance

-- Main function to demonstrate the system
main :: IO ()
main = do
    putStrLn "Starting logging system demo..."
    runApp
    putStrLn "Demo completed. Check the logs directory for output."
