{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------
-- DATUM & REDEEMER
-------------------------------------------------

data DeliveryDatum = DeliveryDatum
    { ddSender     :: PubKeyHash
    , ddReceiver   :: PubKeyHash
    , ddCourier    :: PubKeyHash
    , ddAmount     :: Integer
    , ddDelivered  :: Bool
    }
PlutusTx.unstableMakeIsData ''DeliveryDatum

data DeliveryAction
    = LockFunds
    | ConfirmDelivery
    | ClaimPayment
PlutusTx.unstableMakeIsData ''DeliveryAction

-------------------------------------------------
-- HELPERS
-------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

-------------------------------------------------
-- VALIDATOR
-------------------------------------------------

{-# INLINABLE mkDeliveryValidator #-}
mkDeliveryValidator :: DeliveryDatum -> DeliveryAction -> ScriptContext -> Bool
mkDeliveryValidator dat action ctx =
    case action of

        -------------------------------------------------
        -- Sender locks funds
        -------------------------------------------------
        LockFunds ->
            traceIfFalse "sender not signed" senderSigned

        -------------------------------------------------
        -- Receiver confirms delivery
        -------------------------------------------------
        ConfirmDelivery ->
            traceIfFalse "receiver not signed" receiverSigned &&
            traceIfFalse "already delivered" (not $ ddDelivered dat)

        -------------------------------------------------
        -- Courier claims payment
        -------------------------------------------------
        ClaimPayment ->
            traceIfFalse "courier not signed" courierSigned &&
            traceIfFalse "delivery not confirmed" (ddDelivered dat) &&
            traceIfFalse "courier not paid" courierPaid

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    senderSigned :: Bool
    senderSigned =
        signedBy (ddSender dat) ctx

    receiverSigned :: Bool
    receiverSigned =
        signedBy (ddReceiver dat) ctx

    courierSigned :: Bool
    courierSigned =
        signedBy (ddCourier dat) ctx

    courierPaid :: Bool
    courierPaid =
        valueOf
            (valuePaidTo info (ddCourier dat))
            adaSymbol
            adaToken >= ddAmount dat

-------------------------------------------------
-- UNTYPED WRAPPER
-------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkDeliveryValidator
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator =
    mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- SCRIPT ADDRESS
-------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
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
-- FILE OUTPUT
-------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
    BS.writeFile path (B16.encode bytes)
    putStrLn $ "CBOR hex written to: " <> path

-------------------------------------------------
-- MAIN
-------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "proof_of_delivery.plutus" validator
    writeCBOR      "proof_of_delivery.cbor"   validator

    putStrLn "Proof-of-Delivery validator generated"
