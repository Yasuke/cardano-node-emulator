{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ledger.Test where

import Cardano.Api qualified as C
import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI (policyId)
import Plutus.Script.Utils.Typed as PSU
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.Script.Utils.Value (mpsSymbol)
import PlutusLedgerApi.V1 (Address)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx qualified
import PlutusTx.Prelude (check, BuiltinUnit)
import Prelude hiding (not)

someCode
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> BuiltinUnit)
someCode = $$(PlutusTx.compile [||\_ _ _ -> check False||])

someValidator :: Scripts.Validator
someValidator = Ledger.mkValidatorScript someCode

someTypedValidator :: Scripts.TypedValidator Any
someTypedValidator = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV1)

someValidatorHash :: PV1.ValidatorHash
someValidatorHash = PV1.validatorHash someValidator

someCardanoAddress :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddress = flip PV1.mkValidatorCardanoAddress someValidator

someAddress :: Address
someAddress = Ledger.scriptValidatorHashAddress someValidatorHash Nothing

someValidatorV2 :: Scripts.Validator
someValidatorV2 = Ledger.mkValidatorScript someCode

someTypedValidatorV2 :: Scripts.TypedValidator Any
someTypedValidatorV2 = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV2)

someValidatorHashV2 :: PV2.ValidatorHash
someValidatorHashV2 = PV2.validatorHash someValidatorV2

someCardanoAddressV2 :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddressV2 = flip PV2.mkValidatorCardanoAddress someValidatorV2

someAddressV2 :: Address
someAddressV2 = Ledger.scriptValidatorHashAddress someValidatorHashV2 Nothing

{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> Ledger.ScriptContext -> Bool
mkPolicy _ _ = True

{-# INLINEABLE mkPolicyV2 #-}
mkPolicyV2 :: () -> PV2.ScriptContext -> Bool
mkPolicyV2 _ _ = True

{-# INLINEABLE mkPolicyV3 #-}
mkPolicyV3 :: () -> PV3.ScriptContext -> Bool
mkPolicyV3 _ _ = True

coinMintingPolicy :: Language -> Versioned Ledger.MintingPolicy
coinMintingPolicy lang = case lang of
  PlutusV1 -> Versioned coinMintingPolicyV1 lang
  PlutusV2 -> Versioned coinMintingPolicyV2 lang
  PlutusV3 -> Versioned coinMintingPolicyV3 lang

coinMintingPolicyV1 :: Ledger.MintingPolicy
coinMintingPolicyV1 =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy mkPolicy||])

coinMintingPolicyV2 :: Ledger.MintingPolicy
coinMintingPolicyV2 =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy mkPolicyV2||])

coinMintingPolicyV3 :: Ledger.MintingPolicy
coinMintingPolicyV3 =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||PSU.mkUntypedMintingPolicy mkPolicyV3||])

coinMintingPolicyHash :: Language -> Ledger.MintingPolicyHash
coinMintingPolicyHash = Ledger.mintingPolicyHash . coinMintingPolicy

coinMintingPolicyCurrencySymbol :: Language -> Value.CurrencySymbol
coinMintingPolicyCurrencySymbol = mpsSymbol . coinMintingPolicyHash

someToken :: Language -> Value.Value
someToken lang = Value.singleton (coinMintingPolicyCurrencySymbol lang) "someToken" 1

asRedeemer :: (PlutusTx.ToData a) => a -> Ledger.Redeemer
asRedeemer a = Ledger.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

asDatum :: (PlutusTx.ToData a) => a -> Ledger.Datum
asDatum a = Ledger.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

coinMintingPolicyId :: Language -> C.PolicyId
coinMintingPolicyId = policyId . coinMintingPolicy

testNetworkMagic :: C.NetworkMagic
testNetworkMagic = C.NetworkMagic 1097911063

testnet :: C.NetworkId
testnet = C.Testnet testNetworkMagic
