-- Saviour Uzoukwu
-- 17th March 2025
import Data.Map (Map)
import qualified Data.Map as Map

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
  } deriving (Show)

-- DEX State
data DEX = DEX
  { pools :: [Pool]
  , userBalances :: Map String UserBalances
  } deriving (Show)

-- Initial State
initialDEX :: DEX
initialDEX = DEX
  { pools = []
  , userBalances = Map.fromList [("Alice", Map.fromList [("ETH", 1000), ("DAI", 2000)])
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

-- Swap tokens
swapTokens :: DEX -> String -> Token -> Token -> Amount -> Either String DEX
swapTokens dex user fromToken toToken amount =
  let userBalance = Map.findWithDefault Map.empty user (userBalances dex)
      fromBalance = Map.findWithDefault 0 fromToken userBalance
  in if fromBalance >= amount
     then
       case findPool dex fromToken toToken of
         Just pool ->
           let outputAmount = (reserveB pool * amount) / (reserveA pool + amount)
               updatedReserveA = reserveA pool + amount
               updatedReserveB = reserveB pool - outputAmount
               updatedPool = pool { reserveA = updatedReserveA, reserveB = updatedReserveB }
               updatedPools = map (\p -> if p == pool then updatedPool else p) (pools dex)
               updatedUserBalance = Map.insert fromToken (fromBalance - amount) $
                                    Map.insert toToken (Map.findWithDefault 0 toToken userBalance + outputAmount) userBalance
               updatedUserBalances = Map.insert user updatedUserBalance (userBalances dex)
           in Right $ dex
                { pools = updatedPools
                , userBalances = updatedUserBalances
                }
         Nothing -> Left "No pool found for the token pair"
     else Left "Insufficient balance"

-- Helper function to check if a pool matches the token pair
matchesPool :: Token -> Token -> Pool -> Bool
matchesPool tokenA tokenB pool =
  (tokenA == tokenA pool && tokenB == tokenB pool) || (tokenA == tokenB pool && tokenB == tokenA pool)

-- Find a pool for a token pair
findPool :: DEX -> Token -> Token -> Maybe Pool
findPool dex tokenA tokenB =
  let matchingPools = filter (matchesPool tokenA tokenB) (pools dex)
  in if null matchingPools then Nothing else Just (head matchingPools)

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
  -- Add liquidity
  let Right dex1 = addLiquidity dex "Alice" "ETH" "DAI" 100 1000
  -- Swap tokens
  let Right dex2 = swapTokens dex1 "Bob" "ETH" "DAI" 10
  -- Get Bob's balance
  case getUserBalance dex2 "Bob" of
    Right balance -> print balance
    Left err -> putStrLn err
