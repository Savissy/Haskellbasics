-- Saviour Uzoukwu
-- 17th March 2025
import Data.Map (Map)
import qualified Data.Map as Map

-- Token Definition
type PolicyID = String
type TokenName = String
type Amount = Integer
type UserID = String

data Token = Token
  { tokenName :: TokenName
  , tokenPolicyID :: PolicyID
  } deriving (Show, Eq, Ord)

-- Blockchain State
data Blockchain = Blockchain
  { tokens :: Map (PolicyID, TokenName) Amount  -- Total supply of each token
  , userBalances :: Map UserID (Map Token Amount)  -- Balances of each user
  } deriving (Show)

-- Initial State
initialBlockchain :: Blockchain
initialBlockchain = Blockchain
  { tokens = Map.empty
  , userBalances = Map.empty
  }

-- Mint a new token
mintToken :: Blockchain -> PolicyID -> TokenName -> Amount -> UserID -> Either String Blockchain
mintToken blockchain policyId name amount userId =
  let tokenKey = (policyId, name)
      token = Token name policyId
  in if Map.member tokenKey (tokens blockchain)
     then Left "Token already exists"
     else
       let updatedTokens = Map.insert tokenKey amount (tokens blockchain)
           userBalance = Map.findWithDefault Map.empty userId (userBalances blockchain)
           updatedUserBalance = Map.insert token amount userBalance
           updatedUserBalances = Map.insert userId updatedUserBalance (userBalances blockchain)
       in Right $ blockchain
            { tokens = updatedTokens
            , userBalances = updatedUserBalances
            }

-- Transfer tokens between users
transferTokens :: Blockchain -> UserID -> UserID -> PolicyID -> TokenName -> Amount -> Either String Blockchain
transferTokens blockchain fromUser toUser policyId name amount =
  let tokenKey = (policyId, name)
      token = Token name policyId
      fromBalance = Map.findWithDefault Map.empty fromUser (userBalances blockchain)
      toBalance = Map.findWithDefault Map.empty toUser (userBalances blockchain)
  in case (Map.lookup tokenKey (tokens blockchain), Map.lookup token fromBalance) of
       (Just totalSupply, Just fromAmount) ->
         if fromAmount >= amount
         then
           let updatedFromBalance = Map.insert token (fromAmount - amount) fromBalance
               updatedToBalance = Map.insert token (Map.findWithDefault 0 token toBalance + amount) toBalance
               updatedUserBalances = Map.insert fromUser updatedFromBalance $
                                     Map.insert toUser updatedToBalance (userBalances blockchain)
           in Right $ blockchain { userBalances = updatedUserBalances }
         else Left "Insufficient balance"
       _ -> Left "Token or user not found"

-- Burn tokens
burnTokens :: Blockchain -> UserID -> PolicyID -> TokenName -> Amount -> Either String Blockchain
burnTokens blockchain userId policyId name amount =
  let tokenKey = (policyId, name)
      token = Token name policyId
      userBalance = Map.findWithDefault Map.empty userId (userBalances blockchain)
  in case (Map.lookup tokenKey (tokens blockchain), Map.lookup token userBalance) of
       (Just totalSupply, Just userAmount) ->
         if userAmount >= amount
         then
           let updatedUserBalance = Map.insert token (userAmount - amount) userBalance
               updatedUserBalances = Map.insert userId updatedUserBalance (userBalances blockchain)
               updatedTokens = Map.adjust (\supply -> supply - amount) tokenKey (tokens blockchain)
           in Right $ blockchain
                { tokens = updatedTokens
                , userBalances = updatedUserBalances
                }
         else Left "Insufficient balance"
       _ -> Left "Token or user not found"

-- Get user balance
getUserBalance :: Blockchain -> UserID -> Either String (Map Token Amount)
getUserBalance blockchain userId =
  case Map.lookup userId (userBalances blockchain) of
    Just balance -> Right balance
    Nothing -> Left "User not found"

-- Example Usage
main :: IO ()
main = do
  let blockchain = initialBlockchain
  -- Mint a new token
  let Right blockchain1 = mintToken blockchain "policy1" "MyToken" 1000 "Alice"
  -- Transfer tokens
  let Right blockchain2 = transferTokens blockchain1 "Alice" "Bob" "policy1" "MyToken" 100
  -- Burn tokens
  let Right blockchain3 = burnTokens blockchain2 "Alice" "policy1" "MyToken" 50
  -- Get Alice's balance
  case getUserBalance blockchain3 "Alice" of
    Right balance -> print balance
    Left err -> putStrLn err
