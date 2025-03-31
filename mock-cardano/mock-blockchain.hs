-- Saviour Uzoukwu
-- 17th March 2025
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (randomRIO)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word32)
import Control.Monad (replicateM)

-- Types
type Address = String
type Balance = Int
type Stake = Int
type BlockHash = ByteString
type MerkleRoot = ByteString
type Nonce = Word32
type Difficulty = Word32

-- Transaction
data Transaction = Transaction
  { txFrom :: Address
  , txTo :: Address
  , txAmount :: Balance
  } deriving (Show)

-- Block Header
data BlockHeader = BlockHeader
  { prevHash :: BlockHash
  , merkleRoot :: MerkleRoot
  , timestamp :: UTCTime
  , nonce :: Nonce
  , difficulty :: Difficulty
  } deriving (Show)

-- Block
data Block = Block
  { header :: BlockHeader
  , transactions :: [Transaction]
  , creator :: Address
  } deriving (Show)

-- Blockchain
data Blockchain = Blockchain
  { blocks :: [Block]
  , stakePools :: Map Address Stake
  , balances :: Map Address Balance
  } deriving (Show)

-- Simple hash function
simpleHash :: String -> BlockHash
simpleHash = BS.pack . show . foldl' (\acc c -> acc * 31 + fromEnum c) 0

-- Calculate Merkle root (simplified)
calcMerkleRoot :: [Transaction] -> MerkleRoot
calcMerkleRoot = simpleHash . concatMap show

-- Genesis block
genesisBlock :: Block
genesisBlock = Block
  { header = BlockHeader
      { prevHash = simpleHash "0"
      , merkleRoot = simpleHash ""
      , timestamp = undefined
      , nonce = 0
      , difficulty = 0
      }
  , transactions = []
  , creator = "Genesis"
  }

-- Initial blockchain
initialBlockchain :: Blockchain
initialBlockchain = Blockchain
  { blocks = [genesisBlock]
  , stakePools = Map.fromList [("Pool1", 100), ("Pool2", 200), ("Pool3", 150)]
  , balances = Map.fromList [("Alice", 1000), ("Bob", 500), ("Charlie", 750)]
  }

-- Validate transaction
validateTx :: Transaction -> Map Address Balance -> Bool
validateTx Transaction{..} balances =
  case Map.lookup txFrom balances of
    Just bal -> bal >= txAmount
    Nothing -> False

-- Update balances
updateBalances :: [Transaction] -> Map Address Balance -> Map Address Balance
updateBalances txs balances = foldl' adjust balances txs
  where
    adjust acc Transaction{..} =
      Map.insert txTo (txAmount + Map.findWithDefault 0 txTo acc) $
      Map.insert txFrom (Map.findWithDefault 0 txFrom acc - txAmount) acc

-- Create new block
createBlock :: Blockchain -> [Transaction] -> Address -> IO Block
createBlock bc@Blockchain{..} txs creator = do
  currentTime <- getCurrentTime
  let prevBlock = head blocks
  let validTxs = filter (`validateTx` balances) txs
  return Block
    { header = BlockHeader
        { prevHash = blockHash prevBlock
        , merkleRoot = calcMerkleRoot validTxs
        , timestamp = currentTime
        , nonce = 0  -- Not used in PoS
        , difficulty = 100
        }
    , transactions = validTxs
    , creator = creator
    }

-- Block hash (hashes header only)
blockHash :: Block -> BlockHash
blockHash = simpleHash . show . header

-- Select block creator based on stake
selectCreator :: Blockchain -> IO Address
selectCreator Blockchain{..} = do
  let poolList = Map.toList stakePools
  let totalStake = sum $ map snd poolList
  randomPoint <- randomRIO (1, totalStake)
  return $ selectPool poolList randomPoint
  where
    selectPool ((addr,stake):pools) n
      | n <= stake = addr
      | otherwise = selectPool pools (n - stake)

-- Add block to chain
addBlock :: Blockchain -> Block -> Blockchain
addBlock bc@Blockchain{..} block =
  bc { blocks = block : blocks
     , balances = updateBalances (transactions block) balances
     }

-- Demo: Print blockchain info
printChain :: Blockchain -> IO ()
printChain Blockchain{..} = do
  putStrLn "\n=== Blockchain Summary ==="
  putStrLn $ "Blocks: " ++ show (length blocks)
  putStrLn $ "Stake Pools: " ++ show (Map.toList stakePools)
  putStrLn $ "Balances: " ++ show (Map.toList balances)
  
  putStrLn "\nLatest Block:"
  let latest = head blocks
  putStrLn $ "Creator: " ++ creator latest
  putStrLn $ "Tx Count: " ++ show (length $ transactions latest)
  putStrLn $ "Prev Hash: " ++ BS.unpack (prevHash $ header latest)
  putStrLn $ "Merkle Root: " ++ BS.unpack (merkleRoot $ header latest)

-- Main simulation
main :: IO ()
main = do
  putStrLn "Starting blockchain simulation..."
  
  -- Initialize
  let bc0 = initialBlockchain
  printChain bc0
  
  -- Create some transactions
  now <- getCurrentTime
  let txs = [ Transaction "Alice" "Bob" 100
            , Transaction "Bob" "Charlie" 50
            , Transaction "Charlie" "Alice" 25
            ]
  
  -- Create and add 3 blocks
  bc1 <- addBlocks bc0 txs 3
  printChain bc1
  where
    addBlocks bc _ 0 = return bc
    addBlocks bc txs n = do
      creator <- selectCreator bc
      block <- createBlock bc txs creator
      let newBC = addBlock bc block
      addBlocks newBC txs (n - 1)
