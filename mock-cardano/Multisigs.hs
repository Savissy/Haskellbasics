module Multisig where

import Data.Time.Clock.POSIX (POSIXTime)
import Data.List (nub)

data WalletDatum = WalletDatum
  { signers  :: [String]
  , required :: Int
  , deadline :: POSIXTime
  } deriving Show

newtype WalletRedeemer = Spend [String]
  deriving Show

data TxContext = TxContext
  { actualSigners :: [String]
  , currentTime   :: POSIXTime
  } deriving Show

validate :: WalletDatum -> WalletRedeemer -> TxContext -> Bool
validate (WalletDatum allowed required deadline) (Spend claimed) (TxContext actual now) =
    let valid = filter (\s -> s `elem` allowed && s `elem` actual) claimed
        uniqueValid = nub valid
    in length uniqueValid >= required && now >= deadline
    -- Add test helper functions below
-- ===============================

-- Mock public key hashes for testing
alice :: PubKeyHash
alice = "alice_pkh"  -- In real tests, generate properly

bob :: PubKeyHash
bob = "bob_pkh"

charlie :: PubKeyHash
charlie = "charlie_pkh"

-- Create test datum
testDatum :: WalletDatum
testDatum = WalletDatum
  { signers = [alice, bob, charlie]
  , required = 2
  , deadline = 1700000000  -- POSIX timestamp
  }

-- Create test redeemer
testRedeemer :: WalletRedeemer
testRedeemer = Spend [alice, bob]

-- Create a test context
testContext :: [PubKeyHash] -> POSIXTime -> ScriptContext
testContext signers time = ScriptContext
  { scriptContextTxInfo = TxInfo
      { txInfoSignatories = signers
      , txInfoValidRange = to time
      , txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = mempty
      , txInfoMint = mempty
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoData = []
      , txInfoId = "test_tx_id"
      }
  , scriptContextPurpose = Spending (TxOutRef "test" 0)
  }

-- Test function
runTest :: Bool
runTest = validate testDatum testRedeemer (testContext [alice, bob] 1700000001)
