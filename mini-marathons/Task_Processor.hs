{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TaskProcessor where

import Control.Applicative (Alternative(..))
import Control.Concurrent (threadDelay, forkIO, myThreadId, throwTo)
import Control.Concurrent.Async (async, wait, mapConcurrently)
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar, newTVarIO)
import Control.Monad (forever, when, void, replicateM)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadThrow, MonadCatch, throwM, catch)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (traverse_)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import System.Random (randomRIO)

-- Task types
data Task = Task
  { taskId :: Int
  , taskName :: String
  , taskPriority :: Priority
  , taskPayload :: String
  } deriving (Show, Eq)

data Priority = Low | Medium | High
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Task result
data TaskResult = 
    TaskSuccess String 
  | TaskFailure String 
  | TaskRetry Int String  -- Retry after delay
  deriving (Show, Eq)

-- Task processor configuration
data ProcessorConfig = ProcessorConfig
  { maxConcurrentTasks :: Int
  , maxRetries :: Int
  , baseRetryDelay :: Int  -- microseconds
  , rateLimit :: Int       -- tasks per second
  } deriving (Show)

-- Task processor state
data ProcessorState = ProcessorState
  { completedTasks :: TVar Int
  , failedTasks :: TVar Int
  , retriedTasks :: TVar Int
  , lastRequestTime :: TVar UTCTime
  }

-- Task processor monad
newtype TaskProcessor a = TaskProcessor 
  { runTaskProcessor :: ReaderT (ProcessorConfig, ProcessorState) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

-- Get configuration and state
getConfig :: TaskProcessor ProcessorConfig
getConfig = TaskProcessor $ fst <$> ask

getState :: TaskProcessor ProcessorState
getState = TaskProcessor $ snd <$> ask

-- Rate limiter
rateLimitRequest :: TaskProcessor ()
rateLimitRequest = do
  config <- getConfig
  state <- getState
  now <- liftIO getCurrentTime
  lastTime <- liftIO $ atomically $ readTVar (lastRequestTime state)
  
  let timeDiff = diffUTCTime now lastTime
      minDelay = 1.0 / fromIntegral (rateLimit config)
  
  when (timeDiff < minDelay) $ do
    let delayMicroseconds = floor ((minDelay - timeDiff) * 1000000)
    liftIO $ threadDelay delayMicroseconds
  
  liftIO $ atomically $ writeTVar (lastRequestTime state) now

-- Process a single task
processTask :: Task -> TaskProcessor TaskResult
processTask task = do
  rateLimitRequest
  config <- getConfig
  
  -- Simulate processing with random success/failure
  result <- liftIO $ randomRIO (0, 10 :: Int)
  
  if result > 2  -- 80% success rate
    then do
      liftIO $ putStrLn $ "Task " ++ show (taskId task) ++ " succeeded"
      return $ TaskSuccess ("Processed: " ++ taskName task)
    else if result > 1  -- 10% failure
      then do
        liftIO $ putStrLn $ "Task " ++ show (taskId task) ++ " failed"
        state <- getState
        liftIO $ atomically $ modifyTVar (failedTasks state) (+1)
        return $ TaskFailure ("Failed to process: " ++ taskName task)
      else do  -- 10% retry
        liftIO $ putStrLn $ "Task " ++ show (taskId task) ++ " needs retry"
        state <- getState
        liftIO $ atomically $ modifyTVar (retriedTasks state) (+1)
        return $ TaskRetry (baseRetryDelay config) ("Retrying: " ++ taskName task)

-- Process tasks with retry logic
processTaskWithRetry :: Task -> TaskProcessor TaskResult
processTaskWithRetry task = go 0
  where
    go retryCount = do
      config <- getConfig
      result <- processTask task
      
      case result of
        TaskRetry delay msg -> 
          if retryCount < maxRetries config
            then do
              liftIO $ threadDelay delay
              go (retryCount + 1)
            else return $ TaskFailure $ "Max retries exceeded for: " ++ taskName task
        _ -> return result

-- Process multiple tasks with concurrency control
processTasks :: [Task] -> TaskProcessor [TaskResult]
processTasks tasks = do
  config <- getConfig
  let chunks = chunkList (maxConcurrentTasks config) tasks
  
  results <- traverse processChunk chunks
  return $ concat results
  where
    processChunk chunk = do
      liftIO $ putStrLn $ "Processing chunk of " ++ show (length chunk) ++ " tasks"
      mapConcurrently processTaskWithRetry chunk

    chunkList :: Int -> [a] -> [[a]]
    chunkList _ [] = []
    chunkList n xs = take n xs : chunkList n (drop n xs)

-- Process tasks with different strategies using Functor/Applicative
-- Functor: transform tasks
mapTask :: (String -> String) -> Task -> Task
mapTask f task = task { taskPayload = f (taskPayload task) }

-- Applicative: combine tasks
combineTasks :: Task -> Task -> Task
combineTasks t1 t2 = Task
  { taskId = taskId t1
  , taskName = taskName t1 ++ " & " ++ taskName t2
  , taskPriority = max (taskPriority t1) (taskPriority t2)
  , taskPayload = taskPayload t1 ++ " | " ++ taskPayload t2
  }

-- Alternative: fallback task processing
fallbackProcessing :: Task -> TaskProcessor TaskResult -> TaskProcessor TaskResult
fallbackProcessing task primaryProcessor = 
  primaryProcessor <|> (fallbackProcessor task)
  where
    fallbackProcessor :: Task -> TaskProcessor TaskResult
    fallbackProcessor t = do
      liftIO $ putStrLn $ "Using fallback processing for task " ++ show (taskId t)
      return $ TaskSuccess $ "Fallback processing for: " ++ taskName t

-- Initialize processor state
initProcessorState :: IO ProcessorState
initProcessorState = do
  completed <- newTVarIO 0
  failed <- newTVarIO 0
  retried <- newTVarIO 0
  lastTime <- newTVarIO =<< getCurrentTime
  return $ ProcessorState completed failed retried lastTime

-- Run the task processor
runTaskProcessor :: ProcessorConfig -> ProcessorState -> TaskProcessor a -> IO a
runTaskProcessor config state processor = 
  runReaderT (runTaskProcessor processor) (config, state)

-- Example usage
main :: IO ()
main = do
  putStrLn "Starting task processor..."
  
  -- Create configuration
  let config = ProcessorConfig
        { maxConcurrentTasks = 3
        , maxRetries = 2
        , baseRetryDelay = 100000  -- 100ms
        , rateLimit = 5  -- 5 tasks per second
        }
  
  -- Initialize state
  state <- initProcessorState
  
  -- Create sample tasks
  let tasks = [Task i ("Task " ++ show i) priority ("payload " ++ show i) 
              | i <- [1..10]
              , priority <- [Low, Medium, High]]
  
  -- Process tasks
  results <- runTaskProcessor config state $ processTasks tasks
  
  -- Print results
  putStrLn "Processing results:"
  traverse_ print results
  
  -- Get statistics
  completed <- atomically $ readTVar (completedTasks state)
  failed <- atomically $ readTVar (failedTasks state)
  retried <- atomically $ readTVar (retriedTasks state)
  
  putStrLn $ "Completed: " ++ show completed
  putStrLn $ "Failed: " ++ show failed
  putStrLn $ "Retried: " ++ show retried
  
  putStrLn "Task processing completed!"

-- Helper function to modify TVars
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
  value <- readTVar var
  writeTVar var (f value)
