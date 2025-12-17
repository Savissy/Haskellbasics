here is the updated smart contract with the repayment enforced and it runs successfully: {-# LANGUAGE DataKinds #-}

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

import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)

import PlutusTx

import PlutusTx.Prelude hiding (Semigroup(..), unless)

import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise

import qualified Data.ByteString.Lazy  as LBS

import qualified Data.ByteString.Short as SBS

import qualified Data.ByteString       as BS

import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C

import qualified Cardano.Api.Shelley as CS


---

-- Datum


---

data YieldDatum = YieldDatum

{ ydLender     :: PubKeyHash

, ydBorrower   :: Maybe PubKeyHash   -- ✅ FIX: borrower is optional

, ydPrincipal  :: Integer

, ydInterest   :: Integer

, ydYieldShare :: Integer

}

PlutusTx.unstableMakeIsData ''YieldDatum


---

-- Action


---

data YieldAction

= Deposit

| Borrow Integer

| Repay Integer

| DistributeYield Integer

PlutusTx.unstableMakeIsData ''YieldAction


---

-- Helpers


---

{-# INLINABLE signedBy #-}

signedBy :: PubKeyHash -> ScriptContext -> Bool

signedBy pkh ctx =

txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE anySigner #-}

anySigner :: ScriptContext -> Bool

anySigner ctx =

length (txInfoSignatories (scriptContextTxInfo ctx)) > 0

{-# INLINABLE calcDistribution #-}

calcDistribution :: Integer -> Integer -> (Integer, Integer)

calcDistribution yield ratio =

let lenderPart   = (yield * ratio) `divide` 100

    borrowerPart = yield - lenderPart

in (lenderPart, borrowerPart)

{-# INLINABLE adaPaidTo #-}

adaPaidTo :: TxInfo -> PubKeyHash -> Integer

adaPaidTo info pkh =

valueOf (valuePaidTo info pkh) adaSymbol adaToken

{-# INLINABLE ownInputAda #-}

ownInputAda :: ScriptContext -> Integer

ownInputAda ctx =

case findOwnInput ctx of

    Nothing ->

        traceError "script input missing"



    Just txIn ->

        valueOf

            (txOutValue (txInInfoResolved txIn))

            adaSymbol

            adaToken


---

-- Validator


---

{-# INLINABLE mkYieldValidator #-}

mkYieldValidator :: YieldDatum -> YieldAction -> ScriptContext -> Bool

mkYieldValidator dat action ctx =

case action of



    Deposit ->

        traceIfFalse "lender must sign"

          (signedBy (ydLender dat) ctx)



    Borrow _ ->

        case ydBorrower dat of

            Nothing ->

                -- ✅ First borrower: ANY signer allowed

                traceIfFalse "borrower must sign"

                  (anySigner ctx)



            Just borrower ->

                traceIfFalse "borrower must sign"

                  (signedBy borrower ctx)



    Repay _ ->

        case ydBorrower dat of

            Nothing ->

                traceError "no borrower to repay"



            Just borrower ->

                traceIfFalse "borrower must sign"

                  (signedBy borrower ctx)



    DistributeYield yieldAmt ->

        traceIfFalse "lender must sign"

            (signedBy (ydLender dat) ctx) &&



        case ydBorrower dat of

            Nothing ->

                traceError "no borrower"



            Just borrower ->

                let

                    info = scriptContextTxInfo ctx



                    requiredRepayment =

                        ydPrincipal dat + ydInterest dat



                    scriptAda =

                        ownInputAda ctx



                    (lenderShare, borrowerShare) =

                        calcDistribution yieldAmt (ydYieldShare dat)



                    lenderPaid =

                        adaPaidTo info (ydLender dat)



                    borrowerPaid =

                        adaPaidTo info borrower

                in

                    traceIfFalse "loan not repaid"

                        (scriptAda >= requiredRepayment) &&



                    traceIfFalse "lender not paid correctly"

                        (lenderPaid >= lenderShare) &&



                    traceIfFalse "borrower not paid correctly"

                        (borrowerPaid >= borrowerShare)


---

-- Untyped


---

{-# INLINABLE mkValidatorUntyped #-}

mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()

mkValidatorUntyped d r c =

if mkYieldValidator (unsafeFromBuiltinData d)

                    (unsafeFromBuiltinData r)

                    (unsafeFromBuiltinData c)

then ()

else error ()

validator :: Validator

validator =

mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])


---

-- Script Hash + Address


---

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash

plutusValidatorHash val =

let bytes    = Serialise.serialise val

    short    = SBS.toShort (LBS.toStrict bytes)

    strictBS = SBS.fromShort short

    builtin  = Builtins.toBuiltin strictBS

in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address

plutusScriptAddress =

Address (ScriptCredential (plutusValidatorHash validator)) Nothing


---

-- Bech32


---

toBech32ScriptAddress :: C.NetworkId -> Validator -> String

toBech32ScriptAddress network val =

let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val

    plutusScript = CS.PlutusScriptSerialised serialised

    scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

    shelleyAddr :: C.AddressInEra C.BabbageEra

    shelleyAddr  =

        C.makeShelleyAddressInEra

            network

            (C.PaymentCredentialByScript scriptHash)

            C.NoStakeAddress

in T.unpack (C.serialiseAddress shelleyAddr)


---

-- CBOR HEX


---

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


---

-- Write validator + CBOR


---

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


---

-- Main


---

main :: IO ()

main = do

let network = C.Testnet (C.NetworkMagic 1)

    bech32  = toBech32ScriptAddress network validator

    cborHex = validatorToCBORHex validator



writeValidator "yieldsharing.plutus" validator

writeCBOR      "yieldsharing.cbor"   validator



putStrLn "--- Yield Sharing Lending Contract ---"

putStrLn $ "Bech32 Script Address: " <> bech32

putStrLn $ "CBOR Hex (first 120 chars): " <> P.take 120 cborHex <> "..."

putStrLn "----------------------------------------" now modify the lucid code below to implement the changes made correctly to enforce repayments: import {

Lucid,

Blockfrost,

Constr,

Data,

} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

let lucid;

let walletAddress;

let scriptAddress;

/* ===============================

PLUTUS V2 VALIDATOR (CBOR HEX)

=============================== */

const YIELD_VALIDATOR_CBOR = "<PASTE_YOUR_CBOR_HEX_HERE>";

const script = {

type: "PlutusV2",

script: YIELD_VALIDATOR_CBOR,

};

/* ===============================

INIT LUCID

=============================== */

async function initLucid() {

lucid = await Lucid.new(

new Blockfrost(

  "https://cardano-preprod.blockfrost.io/api/v0",

  "preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip"

),

"Preprod"

);

const api = await window.cardano.lace.enable();

lucid.selectWallet(api);

walletAddress = await lucid.wallet.address();

scriptAddress = lucid.utils.validatorToAddress(script);

log("Connected wallet: " + walletAddress);

log("Script address: " + scriptAddress);

document.getElementById("app").classList.remove("hidden");

}

/* ===============================

DATUM

YieldDatum

Constr 0:

[ lender, borrower(Maybe), principal, interest, yieldShare ]

=============================== */

function mkYieldDatum(lender, borrowerMaybe, principal, interest, yieldShare) {

return Data.to(

new Constr(0, [

  lender,

  borrowerMaybe,           // null OR Constr(0,[pkh])

  BigInt(principal),

  BigInt(interest),

  BigInt(yieldShare),

])

);

}

/* ===============================

REDEEMERS

YieldAction

=============================== */

const redeemerDeposit = () => Data.to(new Constr(0, []));

const redeemerBorrow  = (amt) => Data.to(new Constr(1, [BigInt(amt)]));

const redeemerRepay   = (amt) => Data.to(new Constr(2, [BigInt(amt)]));

const redeemerYield   = (amt) => Data.to(new Constr(3, [BigInt(amt)]));

/* ===============================

DEPOSIT (LENDER)

=============================== */

async function deposit() {

const pkh = lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

const datum = mkYieldDatum(

pkh,

null,          // ✅ NO borrower at deposit

5_000_000,

500_000,

70

);

const tx = await lucid

.newTx()

.payToContract(

  scriptAddress,

  { inline: datum },

  { lovelace: 5_000_000n }

)

.complete();

const signed = await tx.sign().complete();

const txHash = await signed.submit();

log("Deposit TX: " + txHash);

}

/* ===============================

BORROW (FIRST TIME LOCKS BORROWER)

=============================== */

async function borrow() {

const borrowAmt = BigInt(document.getElementById("borrowAmt").value);

const borrowerPkh =

lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

const utxos = await lucid.utxosAt(scriptAddress);

if (utxos.length === 0) return log("No loans available");

// Pick an OPEN loan (ydBorrower = Nothing)

const loanUtxo = utxos.find(u => u.datum !== undefined);

if (!loanUtxo) return log("No usable loan UTXO");

const oldDatum = Data.from(loanUtxo.datum);

const [

lender,

borrower,

principal,

interest,

yieldShare

] = oldDatum.fields;

// ❌ Prevent lender borrowing own loan (extra UI safety)

if (lender === borrowerPkh) {

return log("Lender cannot borrow own funds");

}

// ✅ Lock borrower in datum

const newDatum = mkYieldDatum(

lender,

new Constr(0, [borrowerPkh]), // Just borrower

principal,

interest,

yieldShare

);

const tx = await lucid

.newTx()

.collectFrom([loanUtxo], redeemerBorrow(borrowAmt))

.attachSpendingValidator(script)



// Re-lock remaining funds with UPDATED datum

.payToContract(

  scriptAddress,

  { inline: newDatum },

  { lovelace: loanUtxo.assets.lovelace - borrowAmt }

)



.addSignerKey(borrowerPkh)

.complete();

const signed = await tx.sign().complete();

const txHash = await signed.submit();

log("Borrow TX: " + txHash);

}

/* ===============================

REPAY (BORROWER ONLY)

=============================== */

async function repay() {

const repayAmt = BigInt(document.getElementById("repayAmt").value);

const borrowerPkh =

lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

const utxos = await lucid.utxosAt(scriptAddress);

if (utxos.length === 0) return log("No active loans");

const loanUtxo = utxos.find(u => u.datum !== undefined);

if (!loanUtxo) return log("No loan with datum");

const tx = await lucid

.newTx()

.collectFrom([loanUtxo], redeemerRepay(repayAmt))

.attachSpendingValidator(script)



// Re-lock full value

.payToContract(

  scriptAddress,

  { inline: loanUtxo.datum },

  { lovelace: loanUtxo.assets.lovelace + repayAmt }

)



.addSignerKey(borrowerPkh)

.complete();

const signed = await tx.sign().complete();

const txHash = await signed.submit();

log("Repay TX: " + txHash);

}

/* ===============================

DISTRIBUTE YIELD (LENDER)

=============================== */

async function distributeYield() {

const amt = BigInt(document.getElementById("yieldAmt").value);

const lenderPkh =

lucid.utils.getAddressDetails(walletAddress).paymentCredential.hash;

const utxos = await lucid.utxosAt(scriptAddress);

if (utxos.length === 0) return log("No loans");

const tx = await lucid

.newTx()

.collectFrom(utxos, redeemerYield(amt))

.attachSpendingValidator(script)

.addSignerKey(lenderPkh)

.complete();

const signed = await tx.sign().complete();

const txHash = await signed.submit();

log("Yield distributed: " + txHash);

}

/* ===============================

UI

=============================== */

function log(msg) {

document.getElementById("log").innerText = msg;

}

document.getElementById("connectWallet").onclick = initLucid;

document.getElementById("depositBtn").onclick = deposit;

document.getElementById("borrowBtn").onclick = borrow;

document.getElementById("repayBtn").onclick = repay;

document.getElementById("yieldBtn").onclick = distributeYield;
