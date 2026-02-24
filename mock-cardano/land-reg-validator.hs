{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>), take)
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified PlutusTx.Builtins as Builtins
import qualified Plutus.V1.Ledger.Value as Value

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------
-- DATUM & REDEEMER
-------------------------------------------------

data LandDatum = LandDatum
  { ldParcelId :: BuiltinByteString
  , ldOwnerPkh :: PubKeyHash
  }
PlutusTx.unstableMakeIsData ''LandDatum

data LandRedeemer
  = Transfer PubKeyHash
PlutusTx.unstableMakeIsData ''LandRedeemer

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE landTokenName #-}
landTokenName :: BuiltinByteString -> TokenName
landTokenName pid = TokenName (Builtins.sha2_256 pid)

{-# INLINABLE assetPresent #-}
assetPresent :: Value.Value -> CurrencySymbol -> TokenName -> Integer -> Bool
assetPresent v cs tn amt = Value.valueOf v cs tn == amt

{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
  case getContinuingOutputs ctx of
    [o] -> o
    _   -> traceError "expected exactly 1 continuing output"

{-# INLINABLE outputDatum #-}
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

-------------------------------------------------
-- VALIDATOR (PARAMETERIZED)
-- Params:
--   authorityPkh : PubKeyHash
--   landCs       : CurrencySymbol (policyId of land NFT)
-------------------------------------------------

{-# INLINABLE mkLandValidator #-}
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
    ownerUpdated = ldOwnerPkh outDat == newOwner

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: PubKeyHash -> CurrencySymbol -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped authority landCs d r c =
  if mkLandValidator authority landCs
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
  then ()
  else error ()

validator :: PubKeyHash -> CurrencySymbol -> Validator
validator authority landCs =
  mkValidatorScript $
    $$(PlutusTx.compile [|| \a cs -> mkValidatorUntyped a cs ||])
      `PlutusTx.applyCode` PlutusTx.liftCode authority
      `PlutusTx.applyCode` PlutusTx.liftCode landCs

-------------------------------------------------
-- BECH32 ADDRESS (OFF-CHAIN)
-------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
  let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
      plutusScript :: C.PlutusScript C.PlutusScriptV2
      plutusScript = CS.PlutusScriptSerialised serialised
      scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
      shelleyAddr :: C.AddressInEra C.BabbageEra
      shelleyAddr =
        C.makeShelleyAddressInEra
          network
          (C.PaymentCredentialByScript scriptHash)
          C.NoStakeAddress
  in T.unpack (C.serialiseAddress shelleyAddr)

-------------------------------------------------
-- CBOR HEX (same style as your DAO)
-------------------------------------------------

validatorToCBORHex :: Validator -> String
validatorToCBORHex val =
  let bytes = LBS.toStrict $ Serialise.serialise val
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

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
  LBS.writeFile path (Serialise.serialise val)
  putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
  let bytes = LBS.toStrict (Serialise.serialise val)
      hex   = B16.encode bytes
  BS.writeFile path hex
  putStrLn $ "CBOR hex written to: " <> path

-------------------------------------------------
-- MAIN
-------------------------------------------------

main :: IO ()
main = do
  -- Preprod = Testnet magic 1
  let network = C.Testnet (C.NetworkMagic 1)

  -- Replace with your AUTHORITY PubKeyHash (56 hex chars / 28 bytes)
  let authorityPkh :: PubKeyHash
      authorityPkh = PubKeyHash (toBuiltin (B16.decodeLenient "33414de0df0b747686f8035ee6b8302a87b36b2770f4284c7eef4b26"))

  -- Replace with your land NFT PolicyId / CurrencySymbol (56 hex chars)
  -- This MUST match the policy you compile from LandMintingPolicy.hs
  let landPolicyId :: CurrencySymbol
      landPolicyId = CurrencySymbol (toBuiltin (B16.decodeLenient "20ebbc713d11a2f9ae6a6c1ddbfcf95d107e0c0cdb162e6a2ab44891"))

  let val   = validator authorityPkh landPolicyId
      bech32 = toBech32ScriptAddress network val
      cborH  = validatorToCBORHex val

  writeValidator "land_validator.plutus" val
  writeCBOR      "land_validator.cbor"   val

  putStrLn "\n--- Land Transfer Validator ---"
  putStrLn $ "Bech32 Address: " <> bech32
  putStrLn $ "CBOR (first 120 chars): " <> P.take 120 cborH <> "..."
  putStrLn "-------------------------------"
