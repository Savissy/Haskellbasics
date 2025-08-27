{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module ConfigSystem where

import Control.Applicative (liftA2)
import Control.Monad (when, unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Data.Char (isDigit, isAlpha)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

-- Configuration types
data ConfigValue = 
    CString String 
  | CInt Int 
  | CBool Bool 
  | CList [ConfigValue]
  deriving (Eq, Show)

instance Semigroup ConfigValue where
  (<>) (CList xs) (CList ys) = CList (xs ++ ys)
  (<>) (CList xs) y = CList (xs ++ [y])
  (<>) x (CList ys) = CList (x : ys)
  (<>) x y = CList [x, y]

instance Monoid ConfigValue where
  mempty = CList []

newtype Config = Config { unConfig :: Map String ConfigValue }
  deriving (Eq, Show, Semigroup, Monoid)

-- Functor instance for configuration
instance Functor (Map String) where
  fmap = Map.map

-- Configuration error type
data ConfigError = 
    MissingKey String 
  | InvalidValue String ConfigValue 
  | ParseError String 
  | ValidationError String
  deriving (Eq, Show)

-- Configuration parser monad
newtype ConfigParser a = ConfigParser 
  { runParser :: ExceptT ConfigError (ReaderT Config IO) a }
  deriving (Functor, Applicative, Monad)

-- Helper functions for the parser
getConfig :: ConfigParser Config
getConfig = ConfigParser ask

lookupKey :: String -> ConfigParser ConfigValue
lookupKey key = do
  config <- getConfig
  case Map.lookup key (unConfig config) of
    Just value -> return value
    Nothing -> ConfigParser $ throwError $ MissingKey key

parseString :: String -> ConfigParser String
parseString key = do
  value <- lookupKey key
  case value of
    CString s -> return s
    _ -> ConfigParser $ throwError $ InvalidValue "Expected string" value

parseInt :: String -> ConfigParser Int
parseInt key = do
  value <- lookupKey key
  case value of
    CInt i -> return i
    CString s -> 
      case reads s of
        [(i, "")] -> return i
        _ -> ConfigParser $ throwError $ InvalidValue "Expected integer" value
    _ -> ConfigParser $ throwError $ InvalidValue "Expected integer" value

parseBool :: String -> ConfigParser Bool
parseBool key = do
  value <- lookupKey key
  case value of
    CBool b -> return b
    CString s ->
      case s of
        "true" -> return True
        "false" -> return False
        _ -> ConfigParser $ throwError $ InvalidValue "Expected boolean" value
    _ -> ConfigParser $ throwError $ InvalidValue "Expected boolean" value

-- Validator applicative
newtype Validator a = Validator { runValidator :: a -> Maybe String }

instance Semigroup (Validator a) where
  (Validator f) <> (Validator g) = Validator $ \x -> 
    case f x of
      Nothing -> g x
      Just err -> Just err

instance Monoid (Validator a) where
  mempty = Validator $ const Nothing

validateWith :: (a -> Bool) -> String -> Validator a
validateWith pred errMsg = Validator $ \x -> 
  if pred x then Nothing else Just errMsg

-- Example validators
nonEmpty :: Validator String
nonEmpty = validateWith (not . null) "Value cannot be empty"

validPort :: Validator Int
validPort = validateWith (\p -> p > 0 && p < 65536) "Port must be between 1 and 65535"

alphanumeric :: Validator String
alphanumeric = validateWith (all isAlphaNum) "Value must be alphanumeric"
  where isAlphaNum c = isAlpha c || isDigit c

-- Database configuration
data DatabaseConfig = DatabaseConfig
  { dbHost :: String
  , dbPort :: Int
  , dbName :: String
  , dbUser :: String
  , dbPassword :: String
  } deriving (Show)

-- Parse database config with validation
parseDatabaseConfig :: ConfigParser DatabaseConfig
parseDatabaseConfig = do
  host <- parseString "db_host" `withValidation` nonEmpty
  port <- parseInt "db_port" `withValidation` validPort
  name <- parseString "db_name" `withValidation` nonEmpty
  user <- parseString "db_user" `withValidation` alphanumeric
  password <- parseString "db_password" `withValidation` nonEmpty
  return $ DatabaseConfig host port name user password
  where
    withValidation :: ConfigParser a -> Validator a -> ConfigParser a
    withValidation parser validator = do
      value <- parser
      case runValidator validator value of
        Just err -> ConfigParser $ throwError $ ValidationError err
        Nothing -> return value

-- Load configuration from environment variables
loadEnvConfig :: IO Config
loadEnvConfig = do
  envVars <- sequence [
      ("db_host",) <$> lookupEnv "DB_HOST",
      ("db_port",) <$> fmap (CInt . read) <$> lookupEnv "DB_PORT",
      ("db_name",) <$> lookupEnv "DB_NAME",
      ("db_user",) <$> lookupEnv "DB_USER",
      ("db_password",) <$> lookupEnv "DB_PASSWORD"
    ]
  let configMap = Map.fromList $ catMaybes envVars
  return $ Config configMap
  where
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\x acc -> case x of Just val -> val : acc; Nothing -> acc) []

-- Default configuration
defaultConfig :: Config
defaultConfig = Config $ Map.fromList [
    ("db_host", CString "localhost"),
    ("db_port", CInt 5432),
    ("db_name", CString "mydb")
  ]

-- Merge configurations (left-biased)
mergeConfigs :: Config -> Config -> Config
mergeConfigs (Config defaultMap) (Config envMap) = 
  Config $ Map.union envMap defaultMap

-- Run the configuration parser
runConfigParser :: ConfigParser a -> Config -> IO (Either ConfigError a)
runConfigParser parser config = 
  runReaderT (runExceptT $ runParser parser) config

-- Example usage
main :: IO ()
main = do
  putStrLn "Loading configuration..."
  
  -- Load environment configuration
  envConfig <- loadEnvConfig
  let fullConfig = mergeConfigs defaultConfig envConfig
  
  -- Parse and validate the configuration
  result <- runConfigParser parseDatabaseConfig fullConfig
  
  case result of
    Left err -> do
      hPutStrLn stderr $ "Configuration error: " ++ show err
      exitFailure
    Right dbConfig -> do
      putStrLn "Database configuration:"
      print dbConfig
      putStrLn "Configuration loaded successfully!"
