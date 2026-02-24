{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Prelude (IO, FilePath, putStrLn, (<>))
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified PlutusTx.Builtins as Builtins
import qualified Plutus.V1.Ledger.Value as Value

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Datum: identifies the parcel + current owner (truth of ownership = datum)
data LandDatum = LandDatum
  { ldParcelId  :: BuiltinByteString
  , ldOwnerPkh  :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''LandDatum

data LandRedeemer
  = Transfer PubKeyHash -- new owner PKH
PlutusTx.unstableMakeIsData ''LandRedeemer

-- Minting redeemer: parcelId to enforce tokenName = sha2_256(parcelId)
data MintRedeemer
  = Mint BuiltinByteString
  | Burn BuiltinByteString
PlutusTx.unstableMakeIsData ''MintRedeemer

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINEABLE landTokenName #-}
landTokenName :: BuiltinByteString -> TokenName
landTokenName parcelId = TokenName (Builtins.sha2_256 parcelId)

{-# INLINEABLE assetPresent #-}
assetPresent :: Value.Value -> CurrencySymbol -> TokenName -> Integer -> Bool
assetPresent v cs tn amt = Value.valueOf v cs tn == amt

{-# INLINEABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly 1 continuing output"

{-# INLINEABLE outputDatum #-}
outputDatum :: TxOut -> ScriptContext -> LandDatum
outputDatum o ctx =
  case txOutDatum o of
    OutputDatum (Datum d) ->
      case PlutusTx.fromBuiltinData d of
        Just dat -> dat
        Nothing  -> traceError "bad output datum"
    OutputDatumHash dh ->
      case findDatum dh (scriptContextTxInfo ctx) of
        Just (Datum d) ->
          case PlutusTx.fromBuiltinData d of
            Just dat -> dat
            Nothing  -> traceError "bad output datum (hash)"
        Nothing -> traceError "datum not found"
    NoOutputDatum -> traceError "missing output datum"

--------------------------------------------------------------------------------
-- 1) Minting Policy (Authority-only, tokenName = sha2_256(parcelId))
--------------------------------------------------------------------------------

{-# INLINEABLE mkLandPolicy #-}
mkLandPolicy :: PubKeyHash -> MintRedeemer -> ScriptContext -> Bool
mkLandPolicy authority red ctx =
  traceIfFalse "authority signature missing" signedByAuthority &&
  traceIfFalse "wrong mint/burn" validMintOrBurn
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByAuthority :: Bool
    signedByAuthority = txSignedBy info authority

    -- Enforce exactly +1 or -1 for the expected token name (derived from parcelId).
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

policy :: PubKeyHash -> MintingPolicy
policy authority =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \a -> Scripts.wrapMintingPolicy (mkLandPolicy a) ||])
      `PlutusTx.applyCode` PlutusTx.liftCode authority

policyCurrencySymbol :: PubKeyHash -> CurrencySymbol
policyCurrencySymbol authority = scriptCurrencySymbol (policy authority)

--------------------------------------------------------------------------------
-- 2) Transfer Validator (2-sig: current owner + authority)
--    NFT stays locked at script forever, only datum owner changes.
--------------------------------------------------------------------------------

{-# INLINEABLE mkLandValidator #-}
mkLandValidator :: PubKeyHash -> CurrencySymbol -> LandDatum -> LandRedeemer -> ScriptContext -> Bool
mkLandValidator authority landCs dat red ctx =
  case red of
    Transfer newOwner ->
      traceIfFalse "authority signature missing" signedByAuthority &&
      traceIfFalse "current owner signature missing" signedByOwner &&
      traceIfFalse "NFT missing from input" inputHasNft &&
      traceIfFalse "NFT must remain locked" outputHasNft &&
      traceIfFalse "parcelId must not change" sameParcel &&
      traceIfFalse "owner not updated correctly" ownerUpdated
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    tn :: TokenName
    tn = landTokenName (ldParcelId dat)

    signedByAuthority :: Bool
    signedByAuthority = txSignedBy info authority

    signedByOwner :: Bool
    signedByOwner = txSignedBy info (ldOwnerPkh dat)

    inputHasNft :: Bool
    inputHasNft =
      let vIn = case findOwnInput ctx of
                  Just i  -> txOutValue (txInInfoResolved i)
                  Nothing -> traceError "own input missing"
      in assetPresent vIn landCs tn 1

    out :: TxOut
    out = getContinuingOutput ctx

    outputHasNft :: Bool
    outputHasNft =
      let vOut = txOutValue out
      in assetPresent vOut landCs tn 1

    outDat :: LandDatum
    outDat = outputDatum out ctx

    sameParcel :: Bool
    sameParcel = ldParcelId outDat == ldParcelId dat

    ownerUpdated :: Bool
    ownerUpdated = ldOwnerPkh outDat == case red of Transfer p -> p

validator :: PubKeyHash -> CurrencySymbol -> Validator
validator authority landCs =
  mkValidatorScript $
    $$(PlutusTx.compile [|| \a cs -> Scripts.wrapValidator (mkLandValidator a cs) ||])
      `PlutusTx.applyCode` PlutusTx.liftCode authority
      `PlutusTx.applyCode` PlutusTx.liftCode landCs

validatorHash :: PubKeyHash -> CurrencySymbol -> ValidatorHash
validatorHash a cs = Scripts.validatorHash (validator a cs)

scriptAddress :: PubKeyHash -> CurrencySymbol -> Address
scriptAddress a cs = scriptHashAddress (validatorHash a cs)

--------------------------------------------------------------------------------
-- Export helpers (CBOR writers)
--------------------------------------------------------------------------------

writeCbor :: FilePath -> Script -> IO ()
writeCbor fp s = do
  let serialised = serialiseScript s
  LBS.writeFile fp serialised
  putStrLn ("Wrote: " <> fp)

serialiseScript :: Script -> LBS.ByteString
serialiseScript =
  Serialise.serialise
    . SBS.fromShort
    . SBS.toShort
    . LBS.toStrict
    . Serialise.serialise

-- (Your preference line included for your offchain/Haskell tooling patterns)
shelleyAddr :: C.AddressInEra C.BabbageEra
shelleyAddr = C.ShelleyAddressInEra C.ShelleyBasedEraBabbage (CS.ShelleyAddress CS.Testnet (CS.PaymentCredentialByKey (CS.VerificationKeyHash "00")) CS.NoStakeAddress)

main :: IO ()
main = do
     let network = C.Testnet (C.NetworkMagic 1)
     writeCBOR      "land_registry.cbor"   validator
     putStrLn "LandRegistry: compile with your parameters in your build tooling."
