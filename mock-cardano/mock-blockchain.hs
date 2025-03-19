-- Saviour Uzoukwu
-- 17th March 2025
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.SHA256 (hash)  -- Requires cryptohash-sha256 package
import Control.Monad (replicateM)
import System.Random (randomRIO)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.List (find)

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
  } deriving (Show, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

-- Block structure
data Block = Block
  { blockSlot :: Slot
  , blockTransactions :: [Transaction]
  , blockPrevHash :: BlockHash
  , blockStakePool :: Address
  , blockTimestamp :: UTCTime
  } deriving (Show, Generic)

instance ToJSON Block
instance FromJSON Block

-- Blockchain structure
data Blockchain = Blockchain
  { chainBlocks :: [Block]
  , chainStakePools :: Map Address Stake
  , chainBalances :: Map Address Balance
  } deriving (Show, Generic)

instance ToJSON Blockchain
instance FromJSON Blockchain

-- Smart Contract structure
data SmartContract = SmartContract
  { scAddress :: Address
  , scCode :: String
  } deriving (Show, Generic)

instance ToJSON SmartContract
instance FromJSON SmartContract

-- NFT structure
data NFT = NFT
  { nftId :: String
  , nftOwner :: Address
  } deriving (Show, Generic)

instance ToJSON NFT
instance FromJSON NFT

-- FT structure
data FT = FT
  { ftId :: String
  , ftOwner :: Address
  , ftAmount :: Balance
  } deriving (Show, Generic)

instance ToJSON FT
instance FromJSON FT

-- Genesis block
genesisBlock :: Block
genesisBlock = Block
  { blockSlot = 0
  , blockTransactions = []
  , blockPrevHash = BS.pack "genesis"
  , blockStakePool = "GenesisPool"
  , blockTimestamp = undefined  -- Placeholder, not used
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
hashBlock block = hash (BS.pack (show block))

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

-- Demo smart contract execution
executeSmartContract :: SmartContract -> Blockchain -> Blockchain
executeSmartContract sc blockchain = blockchain  -- Placeholder for smart contract logic

-- Demo NFT creation
createNFT :: Address -> String -> Blockchain -> Blockchain
createNFT owner nftId blockchain = blockchain  -- Placeholder for NFT logic

-- Demo FT creation
createFT :: Address -> String -> Balance -> Blockchain -> Blockchain
createFT owner ftId amount blockchain = blockchain  -- Placeholder for FT logic

-- Main function to demonstrate the blockchain
main :: IO ()
main = do
  let blockchain = initialBlockchain
  creator <- selectBlockCreator blockchain
  currentTime <- getCurrentTime
  let transactions = [Transaction "Alice" "Bob" 100 1 currentTime]
  block <- createBlock blockchain transactions creator
  let newBlockchain = addBlock blockchain block
  print $ queryTip newBlockchain
