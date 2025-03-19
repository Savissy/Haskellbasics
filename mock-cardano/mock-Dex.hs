-- Saviour Uzoukwu
-- 17th March 2025
{-# LANGUAGE OverloadedStrings #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)

-- Token Definition
type Token = String
type Amount = Float

-- User Balances
type UserBalances = Map Token Amount

-- Liquidity Pool
data Pool = Pool
  { tokenA :: Token
  , tokenB :: Token
  , reserveA :: Amount
  , reserveB :: Amount
  } deriving (Show, Eq) -- Derive Eq for Pool

-- DEX State
data DEX = DEX
  { pools :: [Pool]
  , userBalances :: Map String UserBalances
  } deriving (Show)

-- Initial State
initialDEX :: DEX
initialDEX = DEX
  { pools = []
  , userBalances = Map.fromList
      [ ("Alice", Map.fromList [("ETH", 1000), ("DAI", 2000)])
      , ("Bob", Map.fromList [("ETH", 500), ("DAI", 1000)])
      ]
  }

-- Add liquidity to a pool
addLiquidity :: DEX -> String -> Token -> Token -> Amount -> Amount -> Either String DEX
addLiquidity dex user tokenA tokenB amountA amountB =
  let userBalance = Map.findWithDefault Map.empty user (userBalances dex)
      balanceA = Map.findWithDefault 0 tokenA userBalance
      balanceB = Map.findWithDefault 0 tokenB userBalance
  in if balanceA >= amountA && balanceB >= amountB
     then
       let updatedUserBalance = Map.insert tokenA (balanceA - amountA) $
                                Map.insert tokenB (balanceB - amountB) userBalance
           updatedUserBalances = Map.insert user updatedUserBalance (userBalances dex)
           newPool = Pool tokenA tokenB amountA amountB
           updatedPools = newPool : pools dex
       in Right $ dex
            { pools = updatedPools
            , userBalances = updatedUserBalances
            }
     else Left "Insufficient balance"

-- Swap tokens using constant product formula
swapTokens :: DEX -> String -> Token -> Token -> Amount -> Either String DEX
swapTokens dex user fromToken toToken amount =
  let userBalance = Map.findWithDefault Map.empty user (userBalances dex)
      fromBalance = Map.findWithDefault 0 fromToken userBalance
  in if fromBalance < amount
     then Left "Insufficient balance"
     else
       let matchingPools = filter (\pool ->
             (fromToken == tokenA pool && toToken == tokenB pool) ||
             (fromToken == tokenB pool && toToken == tokenA pool)) (pools dex)
       in if null matchingPools
          then Left "No pool found for the token pair"
          else
            let pool = head matchingPools
                k = reserveA pool * reserveB pool -- Constant product
                updatedReserveA = reserveA pool + amount
                updatedReserveB = k / updatedReserveA
                outputAmount = reserveB pool - updatedReserveB
                updatedPool = pool { reserveA = updatedReserveA, reserveB = updatedReserveB }
                updatedPools = map (\p -> if p == pool then updatedPool else p) (pools dex)
                updatedUserBalance = Map.insert fromToken (fromBalance - amount) $
                                     Map.insert toToken (Map.findWithDefault 0 toToken userBalance + outputAmount) userBalance
                updatedUserBalances = Map.insert user updatedUserBalance (userBalances dex)
            in Right $ dex
                 { pools = updatedPools
                 , userBalances = updatedUserBalances
                 }

-- Get user balance
getUserBalance :: DEX -> String -> Either String UserBalances
getUserBalance dex user =
  case Map.lookup user (userBalances dex) of
    Just balance -> Right balance
    Nothing -> Left "User not found"

-- Example Usage
main :: IO ()
main = do
  let dex = initialDEX

  -- Alice adds liquidity to the ETH-DAI pool
  let Right dex1 = addLiquidity dex "Alice" "ETH" "DAI" 100 1000

  -- Bob swaps 10 ETH for DAI
  let Right dex2 = swapTokens dex1 "Bob" "ETH" "DAI" 10

  -- Check Bob's balance after the swap
  case getUserBalance dex2 "Bob" of
    Right balance -> print $ "Bob's balance: " ++ show balance
    Left err -> putStrLn err
