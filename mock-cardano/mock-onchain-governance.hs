-- Saviour Uzoukwu
-- 17th March 2025
{-# LANGUAGE OverloadedStrings #-}
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Control.Monad (when)

-- Types
type UserID = String
type ProposalID = Int
type Amount = Integer
type Vote = Bool -- True = Yes, False = No

-- Proposal Definition
data Proposal = Proposal
  { proposalID :: ProposalID
  , description :: Text
  , amount :: Amount
  , recipient :: UserID
  , votesFor :: Amount
  , votesAgainst :: Amount
  , executed :: Bool
  } deriving (Show)

-- Governance State
data Governance = Governance
  { proposals :: Map ProposalID Proposal
  , userBalances :: Map UserID Amount
  , treasury :: Amount
  , nextProposalID :: ProposalID
  } deriving (Show)

-- Initial State
initialGovernance :: Governance
initialGovernance = Governance
  { proposals = Map.empty
  , userBalances = Map.fromList [("Alice", 1000), ("Bob", 500), ("Charlie", 300)]
  , treasury = 10000
  , nextProposalID = 1
  }

-- Create a new proposal
createProposal :: Governance -> UserID -> Text -> Amount -> UserID -> Either String Governance
createProposal gov userId description amount recipient =
  if amount > treasury gov
  then Left "Insufficient treasury funds"
  else
    let newProposal = Proposal
          { proposalID = nextProposalID gov
          , description = description
          , amount = amount
          , recipient = recipient
          , votesFor = 0
          , votesAgainst = 0
          , executed = False
          }
        updatedProposals = Map.insert (nextProposalID gov) newProposal (proposals gov)
        updatedNextID = nextProposalID gov + 1
    in Right $ gov
         { proposals = updatedProposals
         , nextProposalID = updatedNextID
         }

-- Vote on a proposal
voteOnProposal :: Governance -> UserID -> ProposalID -> Vote -> Either String Governance
voteOnProposal gov userId proposalId vote =
  case (Map.lookup userId (userBalances gov), Map.lookup proposalId (proposals gov)) of
    (Just balance, Just proposal) ->
      let weight = balance
          updatedProposal = if vote
                            then proposal { votesFor = votesFor proposal + weight }
                            else proposal { votesAgainst = votesAgainst proposal + weight }
          updatedProposals = Map.insert proposalId updatedProposal (proposals gov)
      in Right $ gov { proposals = updatedProposals }
    _ -> Left "User or proposal not found"

-- Execute a proposal
executeProposal :: Governance -> ProposalID -> Either String Governance
executeProposal gov proposalId =
  case Map.lookup proposalId (proposals gov) of
    Just proposal ->
      let totalVotes = votesFor proposal + votesAgainst proposal
          quorum = 500 -- Minimum total votes required
          threshold = 60 -- Minimum approval percentage
          approvalRate = (votesFor proposal * 100) `div` totalVotes
      in if totalVotes >= quorum && approvalRate >= threshold && not (executed proposal)
         then
           let updatedTreasury = treasury gov - amount proposal
               updatedProposal = proposal { executed = True }
               updatedProposals = Map.insert proposalId updatedProposal (proposals gov)
               updatedBalances = Map.adjust (+ amount proposal) (recipient proposal) (userBalances gov)
           in Right $ gov
                { treasury = updatedTreasury
                , proposals = updatedProposals
                , userBalances = updatedBalances
                }
         else Left "Proposal did not pass or already executed"
    Nothing -> Left "Proposal not found"

-- Example Usage
main :: IO ()
main = do
  let gov = initialGovernance
  -- Create a proposal
  let Right gov1 = createProposal gov "Alice" "Fund community project" 1000 "Bob"
  -- Vote on the proposal
  let Right gov2 = voteOnProposal gov1 "Alice" 1 True
  let Right gov3 = voteOnProposal gov2 "Bob" 1 False
  let Right gov4 = voteOnProposal gov3 "Charlie" 1 True
  -- Execute the proposal
  case executeProposal gov4 1 of
    Right gov5 -> do
      putStrLn "Proposal executed successfully!"
      print gov5
    Left err -> putStrLn $ "Error: " ++ err
