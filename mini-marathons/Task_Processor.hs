{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TaskProcessor where

import Control.Applicative (Alternative(..))
import Control.Concurrent (threadDelay)
import Control.Monad (when, replicateM)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (MonadIO(..))
import Data.Foldable (traverse_)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Control.Concurrent.STM
  ( TVar, STM, atomically, readTVar, writeTVar, newTVarIO )

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
  deriving (Functor, Applicative, Monad, MonadIO)

-- Helpers to access config and state
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

  let timeDiff = realToFrac (diffUTCTime now lastTime) :: Double
      minDelay = 1.0 / fromIntegral (rateLimit config)

  when (timeDiff < minDelay) $ do
    let delayMicroseconds = floor ((minDelay - timeDiff) * 1e6)
    liftIO $ threadDelay delayMicroseconds

  liftIO $ atomically $ writeTVar (lastRequestTime state) now

-- Simulate processing a task
processTask :: Task -> TaskProcessor TaskResult
processTask task = do
  rateLimitRequest
  config <- getConfig

  -- Simulate result deterministically
  let result = taskId task `mod` 10

  if result > 2
    then do
      liftIO $ putStrLn $ "Task " ++ show (taskId task) ++ " succeeded"
      state <- getState
      liftIO $ atomically $ modifyTVar (completedTasks state) (+1)
      return $ TaskSuccess ("Processed: " ++ taskName task)
  else if result > 1
    then do
      liftIO $ putStrLn $ "Task " ++ show (taskId task) ++ " failed"
      state <- getState
      liftIO $ atomically $ modifyTVar (failedTasks state) (+1)
      return $ TaskFailure ("Failed: " ++ taskName task)
  else do
      liftIO $ putStrLn $ "Task " ++ show (taskId task) ++ " needs retry"
      state <- getState
      liftIO $ atomically $ modifyTVar (retriedTasks state) (+1)
      return $ TaskRetry (baseRetryDelay config) ("Retrying: " ++ taskName task)

-- Retry logic
processTaskWithRetry :: Task -> TaskProcessor TaskResult
processTaskWithRetry task = go 0
  where
    go retryCount = do
      config <- getConfig
      result <- processTask task
      case result of
        TaskRetry delay _ ->
          if retryCount < maxRetries config
            then do
              liftIO $ threadDelay delay
              go (retryCount + 1)
            else return $ TaskFailure $ "Max retries exceeded for: " ++ taskName task
        _ -> return result

-- Process list of tasks (sequential fallback)
processTasks :: [Task] -> TaskProcessor [TaskResult]
processTasks tasks = do
  config <- getConfig
  let chunks = chunkList (maxConcurrentTasks config) tasks
  results <- mapM processChunk chunks
  return (concat results)
  where
    processChunk chunk = do
      liftIO $ putStrLn $ "Processing chunk of " ++ show (length chunk) ++ " tasks"
      mapM processTaskWithRetry chunk

    chunkList _ [] = []
    chunkList n xs = take n xs : chunkList n (drop n xs)

-- Modify TVar helper
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = readTVar var >>= writeTVar var . f

-- Initialize processor state
initProcessorState :: IO ProcessorState
initProcessorState = do
  completed <- newTVarIO 0
  failed <- newTVarIO 0
  retried <- newTVarIO 0
  lastTime <- newTVarIO =<< getCurrentTime
  return $ ProcessorState completed failed retried lastTime

-- Updated to avoid name clash
runTaskProcessorIO :: ProcessorConfig -> ProcessorState -> TaskProcessor a -> IO a
runTaskProcessorIO config state processor =
  runReaderT (runTaskProcessor processor) (config, state)

-- Entry point
main :: IO ()
main = do
  putStrLn "Starting task processor..."

  let config = ProcessorConfig
        { maxConcurrentTasks = 3
        , maxRetries = 2
        , baseRetryDelay = 100000  -- 100ms
        , rateLimit = 5
        }

  state <- initProcessorState

  -- Sample tasks
  let tasks = [ Task i ("Task " ++ show i) p ("payload " ++ show i)
              | (i, p) <- zip [1..10] (cycle [Low, Medium, High]) ]

  results <- runTaskProcessorIO config state $ processTasks tasks

  putStrLn "\nProcessing results:"
  traverse_ print results

  completed <- atomically $ readTVar (completedTasks state)
  failed    <- atomically $ readTVar (failedTasks state)
  retried   <- atomically $ readTVar (retriedTasks state)

  putStrLn $ "\nCompleted: " ++ show completed
  putStrLn $ "Failed: " ++ show failed
  putStrLn $ "Retried: " ++ show retried
  putStrLn "Task processing completed!"
