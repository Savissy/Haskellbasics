{-# LANGUAGE OverloadedStrings #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)
import Text.Printf (printf)

-- Token Definition
type TokenName = String
type Amount = Float

data Token = Token
  { tokenName :: TokenName
  , totalSupply :: Amount
  } deriving (Show)

-- Liquidity Pool Definition
data Pool = Pool
  { tokenA :: TokenName
  , tokenB :: TokenName
  , reserveA :: Amount
  , reserveB :: Amount
  } deriving (Show)

-- User Definition
type UserName = String
type UserBalances = Map TokenName Amount

data User = User
  { userName :: UserName
  , balances :: UserBalances
  } deriving (Show)

-- DEX State
data DEX = DEX
  { tokens :: Map TokenName Token
  , pools :: [Pool]
  , users :: Map UserName User
  } deriving (Show)

-- Initial State
initialDEX :: DEX
initialDEX = DEX
  { tokens = Map.empty
  , pools = []
  , users = Map.empty
  }

-- Helper Functions
getUser :: DEX -> UserName -> Maybe User
getUser dex name = Map.lookup name (users dex)

getToken :: DEX -> TokenName -> Maybe Token
getToken dex name = Map.lookup name (tokens dex)

-- Create a new token
createToken :: DEX -> UserName -> TokenName -> Amount -> Either String DEX
createToken dex user name supply =
  if Map.member name (tokens dex)
  then Left "Token already exists"
  else
    let newToken = Token name supply
        updatedTokens = Map.insert name newToken (tokens dex)
        updatedUsers = Map.adjust (\u -> u { balances = Map.insert name supply (balances u) }) user (users dex)
    in Right $ dex { tokens = updatedTokens, users = updatedUsers }

-- Add a new user
addUser :: DEX -> UserName -> Either String DEX
addUser dex name =
  if Map.member name (users dex)
  then Left "User already exists"
  else
    let newUser = User name Map.empty
        updatedUsers = Map.insert name newUser (users dex)
    in Right $ dex { users = updatedUsers }

-- Provide liquidity to a pool
provideLiquidity :: DEX -> UserName -> TokenName -> TokenName -> Amount -> Amount -> Either String DEX
provideLiquidity dex user tokenA tokenB amountA amountB =
  case (getUser dex user, getToken dex tokenA, getToken dex tokenB) of
    (Just userObj, Just tokA, Just tokB) ->
      let userBalances = balances userObj
          balanceA = Map.findWithDefault 0 tokenA userBalances
          balanceB = Map.findWithDefault 0 tokenB userBalances
      in if balanceA >= amountA && balanceB >= amountB
         then
           let updatedBalances = Map.insert tokenA (balanceA - amountA) $
                                 Map.insert tokenB (balanceB - amountB) userBalances
               updatedUser = userObj { balances = updatedBalances }
               newPool = Pool tokenA tokenB amountA amountB
               updatedPools = newPool : pools dex
               updatedUsers = Map.insert user userName updatedUser (users dex)
           in Right $ dex { pools = updatedPools, users = updatedUsers }
         else Left "Insufficient balance"
    _ -> Left "User or token not found"

-- Swap tokens
swapTokens :: DEX -> UserName -> TokenName -> TokenName -> Amount -> Either String DEX
swapTokens dex user fromToken toToken amount =
  case (getUser dex user, getToken dex fromToken, getToken dex toToken) of
    (Just userObj, Just fromTok, Just toTok) ->
      let userBalances = balances userObj
          fromBalance = Map.findWithDefault 0 fromToken userBalances
      in if fromBalance >= amount
         then
           case findPool dex fromToken toToken of
             Just pool ->
               let outputAmount = (reserveB pool * amount) / (reserveA pool + amount)
                   updatedReserveA = reserveA pool + amount
                   updatedReserveB = reserveB pool - outputAmount
                   updatedPool = pool { reserveA = updatedReserveA, reserveB = updatedReserveB }
                   updatedPools = map (\p -> if p == pool then updatedPool else p) (pools dex)
                   updatedBalances = Map.insert fromToken (fromBalance - amount) $
                                     Map.insert toToken (Map.findWithDefault 0 toToken userBalances + outputAmount) userBalances
                   updatedUser = userObj { balances = updatedBalances }
                   updatedUsers = Map.insert user userName updatedUser (users dex)
               in Right $ dex { pools = updatedPools, users = updatedUsers }
             Nothing -> Left "No pool found for the token pair"
         else Left "Insufficient balance"
    _ -> Left "User or token not found"

-- Find a pool for a token pair
findPool :: DEX -> TokenName -> TokenName -> Maybe Pool
findPool dex tokenA tokenB =
  let matchingPools = filter (\p -> (tokenA == tokenA p && tokenB == tokenB p) || (tokenA == tokenB p && tokenB == tokenA p)) (pools dex)
  in if null matchingPools then Nothing else Just (head matchingPools)

-- Main Function (Example Usage)
main :: IO ()
main = do
  let dex = initialDEX
  -- Add users
  let Right dex1 = addUser dex "Alice"
  let Right dex2 = addUser dex1 "Bob"
  -- Create tokens
  let Right dex3 = createToken dex2 "Alice" "ETH" 1000
  let Right dex4 = createToken dex3 "Bob" "DAI" 2000
  -- Provide liquidity
  let Right dex5 = provideLiquidity dex4 "Alice" "ETH" "DAI" 100 1000
  -- Swap tokens
  let Right dex6 = swapTokens dex5 "Bob" "DAI" "ETH" 100
  -- Print final state
  print dex6
