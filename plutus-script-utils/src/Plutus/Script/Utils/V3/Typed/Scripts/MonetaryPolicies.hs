{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Script.Utils.V3.Typed.Scripts.MonetaryPolicies (
  mkForwardingMintingPolicy,
  forwardToValidator,
) where

import Plutus.Script.Utils.Scripts (
  MintingPolicy,
  ValidatorHash (ValidatorHash),
  mkMintingPolicyScript,
 )
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V2.Tx (TxOut (TxOut, txOutAddress))
import PlutusLedgerApi.V3 (
  Address (Address, addressCredential),
  Credential (ScriptCredential),
  ScriptHash (ScriptHash),
  txInInfoResolved,
 )
import PlutusLedgerApi.V3.Contexts (
  ScriptContext (ScriptContext, scriptContextTxInfo),
  TxInfo (TxInfo, txInfoInputs),
 )
import PlutusLedgerApi.V3.Contexts qualified as PV3
import PlutusTx qualified
import PlutusTx.Prelude (BuiltinUnit, Bool (False), any, ($), (.), (==))

-- TODO: we should add a TypedMintingPolicy interface here

{- | A minting policy that checks whether the validator script was run
  in the minting transaction.
-}
mkForwardingMintingPolicy :: ValidatorHash -> MintingPolicy
mkForwardingMintingPolicy vshsh =
  mkMintingPolicyScript
    $ $$( PlutusTx.compile
            [||
            \(hsh :: ValidatorHash) ->
              mkUntypedMintingPolicy (forwardToValidator hsh)
            ||]
        )
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 vshsh

{-# INLINEABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> BuiltinUnit -> PV3.ScriptContext -> Bool
forwardToValidator _ _ _ = False
