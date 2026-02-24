{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>), take)
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins
import qualified Plutus.V1.Ledger.Value as Value

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C

-------------------------------------------------
-- REDEEMER
-------------------------------------------------

-- Redeemer carries parcelId so we can enforce tokenName = sha2_256(parcelId)
data MintRedeemer
  = Mint BuiltinByteString
  | Burn BuiltinByteString
PlutusTx.unstableMakeIsData ''MintRedeemer

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE landTokenName #-}
landTokenName :: BuiltinByteString -> TokenName
landTokenName pid = TokenName (Builtins.sha2_256 pid)

-------------------------------------------------
-- MINTING POLICY
-------------------------------------------------

{-# INLINABLE mkLandPolicy #-}
mkLandPolicy :: PubKeyHash -> MintRedeemer -> ScriptContext -> Bool
mkLandPolicy authority red ctx =
  traceIfFalse "authority signature missing" (txSignedBy info authority) &&
  traceIfFalse "wrong mint/burn" validMintOrBurn
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    validMintOrBurn :: Bool
    validMintOrBurn =
      case red of
        Mint pid ->
          let tn  = landTokenName pid
              cs  = ownCurrencySymbol ctx
              amt = Value.valueOf (txInfoMint info) cs tn
          in traceIfFalse "must mint exactly 1" (amt == 1)

        Burn pid ->
          let tn  = landTokenName pid
              cs  = ownCurrencySymbol ctx
              amt = Value.valueOf (txInfoMint info) cs tn
          in traceIfFalse "must burn exactly 1" (amt == (-1))

{-# INLINABLE mkPolicyUntyped #-}
mkPolicyUntyped :: PubKeyHash -> BuiltinData -> BuiltinData -> ()
mkPolicyUntyped authority r c =
  if mkLandPolicy authority (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)
  then ()
  else error ()

policy :: PubKeyHash -> MintingPolicy
policy authority =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \a -> mkPolicyUntyped a ||])
      `PlutusTx.applyCode` PlutusTx.liftCode authority

policyId :: PubKeyHash -> CurrencySymbol
policyId authority = scriptCurrencySymbol (policy authority)

-------------------------------------------------
-- CBOR HEX (same style as your DAO)
-------------------------------------------------

policyToCBORHex :: MintingPolicy -> String
policyToCBORHex pol =
  let bytes = LBS.toStrict $ Serialise.serialise pol
  in BS.foldr (\b acc -> byteToHex b <> acc) "" bytes
  where
    hexChars = "0123456789abcdef"
    byteToHex b =
      let hi = P.fromIntegral b `P.div` 16
          lo = P.fromIntegral b `P.mod` 16
      in [ hexChars P.!! hi, hexChars P.!! lo ]

-------------------------------------------------
-- FILE WRITERS
-------------------------------------------------

writePolicy :: FilePath -> MintingPolicy -> IO ()
writePolicy path pol = do
  LBS.writeFile path (Serialise.serialise pol)
  putStrLn $ "Policy written to: " <> path

writePolicyCBOR :: FilePath -> MintingPolicy -> IO ()
writePolicyCBOR path pol = do
  let bytes = LBS.toStrict (Serialise.serialise pol)
      hex   = B16.encode bytes
  BS.writeFile path hex
  putStrLn $ "CBOR hex written to: " <> path

-------------------------------------------------
-- MAIN
-------------------------------------------------

main :: IO ()
main = do
  -- Replace with your AUTHORITY PubKeyHash (56 hex chars / 28 bytes)
  -- You can get it off-chain via Lucid:
  -- lucid.utils.getAddressDetails(AUTHORITY_ADDRESS).paymentCredential.hash
  let authorityPkh :: PubKeyHash
      authorityPkh = PubKeyHash (toBuiltin (B16.decodeLenient "33414de0df0b747686f8035ee6b8302a87b36b2770f4284c7eef4b26"))

  let pol = policy authorityPkh
      cs  = policyId authorityPkh
      cborH = policyToCBORHex pol

  writePolicy     "land_policy.plutus" pol
  writePolicyCBOR "land_policy.cbor"   pol

  putStrLn "\n--- Land Minting Policy ---"
  putStrLn $ "PolicyId (CurrencySymbol): " <> P.show cs
  putStrLn $ "CBOR (first 120 chars): " <> P.take 120 cborH <> "..."
  putStrLn "---------------------------"
