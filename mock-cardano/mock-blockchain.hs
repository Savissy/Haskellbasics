-- Saviour Uzoukwu
-- 17th March 2025
module Main where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Define types
type Address = String
type Balance = Int
type Stake = Int
type BlockHash = ByteString
type TransactionHash = ByteString
type Slot = Int

-- Transaction structure
data Transaction = Transaction
  { txFrom :: Address
  , txTo :: Address
  , txAmount :: Balance
  , txFee :: Balance
  , txTimestamp :: UTCTime
  } deriving (Show)

-- Block structure
data Block = Block
  { blockSlot :: Slot
  , blockTransactions :: [Transaction]
  , blockPrevHash :: BlockHash
  , blockStakePool :: Address
  , blockTimestamp :: UTCTime
  } deriving (Show)

-- Blockchain structure
data Blockchain = Blockchain
  { chainBlocks :: [Block]
  , chainStakePools :: Map Address Stake
  , chainBalances :: Map Address Balance
  } deriving (Show)

-- Simple hash function (for demonstration only - not cryptographically secure)
simpleHash :: String -> BlockHash
simpleHash = BS.pack . show . foldl (\acc c -> acc * 31 + fromEnum c) 0

-- Genesis block
genesisBlock :: Block
genesisBlock = Block
  { blockSlot = 0
  , blockTransactions = []
  , blockPrevHash = simpleHash "genesis"
  , blockStakePool = "GenesisPool"
  , blockTimestamp = undefined  -- Placeholder
  }

-- Initial blockchain state
initialBlockchain :: Blockchain
initialBlockchain = Blockchain
  { chainBlocks = [genesisBlock]
  , chainStakePools = Map.fromList [("Pool1", 100), ("Pool2", 200)]
  , chainBalances = Map.fromList [("Alice", 1000), ("Bob", 500)]
  }

-- Hash a block
hashBlock :: Block -> BlockHash
hashBlock block = simpleHash (show block)

-- Add a new block to the blockchain
addBlock :: Blockchain -> Block -> Blockchain
addBlock blockchain block =
  blockchain { chainBlocks = block : chainBlocks blockchain }

-- Select a random stake pool to create a block
selectBlockCreator :: Blockchain -> IO Address
selectBlockCreator blockchain = do
  let stakePools = Map.toList (chainStakePools blockchain)
  let totalStake = sum (map snd stakePools)
  randomStake <- randomRIO (1, totalStake)
  return $ selectStakePool stakePools randomStake
  where
    selectStakePool ((addr, stake):rest) remainingStake
      | remainingStake <= stake = addr
      | otherwise = selectStakePool rest (remainingStake - stake)

-- Create a new block
createBlock :: Blockchain -> [Transaction] -> Address -> IO Block
createBlock blockchain transactions creator = do
  currentTime <- getCurrentTime
  let prevBlock = head (chainBlocks blockchain)
  let prevHash = hashBlock prevBlock
  let slot = blockSlot prevBlock + 1
  return Block
    { blockSlot = slot
    , blockTransactions = transactions
    , blockPrevHash = prevHash
    , blockStakePool = creator
    , blockTimestamp = currentTime
    }

-- Query the tip of the blockchain
queryTip :: Blockchain -> Block
queryTip = head . chainBlocks

-- Main function to demonstrate the blockchain
main :: IO ()
main = do
  let blockchain = initialBlockchain
  putStrLn "Initial blockchain state:"
  print blockchain
  
  putStrLn "\nSelecting block creator..."
  creator <- selectBlockCreator blockchain
  putStrLn $ "Selected creator: " ++ creator
  
  putStrLn "\nCreating transaction..."
  currentTime <- getCurrentTime
  let transactions = [Transaction "Alice" "Bob" 100 1 currentTime]
  
  putStrLn "\nCreating new block..."
  block <- createBlock blockchain transactions creator
  
  putStrLn "\nAdding block to blockchain..."
  let newBlockchain = addBlock blockchain block
  
  putStrLn "\nNew blockchain state:"
  print newBlockchain
  
  putStrLn "\nCurrent tip of the blockchain:"
  print (queryTip newBlockchain)
