{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test facility for 'Cardano.Node.Emulator.API.MonadEmulator'
module Cardano.Node.Emulator.Test (
  -- * Basic testing
  testnet,
  hasValidatedTransactionCountOfTotal,
  renderLogs,

  -- * Testing with `quickcheck-contractmodel`
  propSanityCheckModel,
  propSanityCheckAssertions,
  propRunActions_,
  propRunActions,
  propRunActionsWithOptions,
  checkThreatModelWithOptions,
  checkDoubleSatisfactionWithOptions,
  defInitialDist,
  balanceChangePredicate,

  -- * Other exports
  chainStateToChainIndex,
  chainStateToContractModelChainState,

  -- * Re-export quickcheck-contractmodel
  module Test.QuickCheck.ContractModel,
) where

import Cardano.Api qualified as C
import Cardano.Api qualified as CardanoAPI
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.API (
  EmulatorLogs,
  EmulatorM,
  EmulatorMsg (ChainEvent),
  LogMessage (LogMessage),
  awaitSlot,
  emptyEmulatorStateWithInitialDist,
  esChainState,
  getParams,
 )
import Cardano.Node.Emulator.Generators (knownAddresses)
import Cardano.Node.Emulator.Internal.Node qualified as E
import Cardano.Node.Emulator.Internal.Node.Params (ledgerProtocolParameters, pNetworkId, testnet)
import Control.Lens (use, view, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.Strict (evalRWS)
import Control.Monad.Writer (runWriterT)
import Data.Default (def)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (Sum))
import Data.Text qualified as Text
import Ledger (
  CardanoAddress,
  CardanoTx (CardanoEmulatorEraTx),
  OnChainTx,
  onChainTxIsValid,
  unOnChain,
 )
import Ledger.Index qualified as Index
import Ledger.Tx.CardanoAPI (fromCardanoSlotNo)
import Ledger.Value.CardanoAPI qualified as Value
import Plutus.Script.Utils.Value (Value)
import PlutusTx.Builtins qualified as Builtins
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty
import Test.QuickCheck as QC (Property, Testable (property), counterexample, expectFailure, (.&&.))
import Test.QuickCheck.ContractModel (
  Actions,
  BalanceChangeOptions (BalanceChangeOptions),
  ChainIndex (ChainIndex, networkId, transactions),
  ChainState (ChainState, slot, utxo),
  ContractModel,
  HasChainIndex,
  IsRunnable,
  ModelState,
  RunModel,
  RunMonad (unRunMonad),
  TxInState (TxInState),
  assertBalanceChangesMatch,
  asserts,
  balanceChanges,
  runContractModel,
  signerPaysFees,
  stateAfter,
  symIsZero,
 )
import Test.QuickCheck.ContractModel qualified as CM
import Test.QuickCheck.ContractModel qualified as QCCM
import Test.QuickCheck.ContractModel.Internal (ContractModelResult)
import Test.QuickCheck.ContractModel.ThreatModel (ThreatModel, assertThreatModel)
import Test.QuickCheck.Monadic (PropertyM, monadic, monadicIO)
import Test.QuickCheck.StateModel (Realized)
import Test.QuickCheck.StateModel qualified as QCSM
import Test.QuickCheck.ThreatModel.DoubleSatisfaction (doubleSatisfaction)

{- | Test the number of validated transactions and the total number of transactions.
Returns a failure message if the numbers don't match up.
-}
hasValidatedTransactionCountOfTotal :: Int -> Int -> EmulatorLogs -> Maybe String
hasValidatedTransactionCountOfTotal valid total lg =
  let count = \case
        LogMessage _ (ChainEvent (E.TxnValidation Index.Success{})) -> (Sum 1, Sum 0)
        LogMessage _ (ChainEvent (E.TxnValidation Index.FailPhase1{})) -> (Sum 0, Sum 1)
        LogMessage _ (ChainEvent (E.TxnValidation Index.FailPhase2{})) -> (Sum 0, Sum 1)
        _otherLogMsg -> mempty
      (Sum validCount, Sum invalidCount) = foldMap count lg
   in if valid /= validCount
        then Just $ "Unexpected number of valid transactions: " ++ show validCount
        else
          if total - valid /= invalidCount
            then Just $ "Unexpected number of invalid transactions: " ++ show invalidCount
            else Nothing

-- | Render the logs in a format useful for debugging why a test failed.
renderLogs :: EmulatorLogs -> Text.Text
renderLogs =
  Pretty.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions
    . Pretty.vsep
    . toList
    . fmap Pretty.pretty

type instance Realized EmulatorM a = a

instance IsRunnable EmulatorM where
  awaitSlot = awaitSlot . fromCardanoSlotNo

instance HasChainIndex EmulatorM where
  getChainIndex = do
    nid <- pNetworkId <$> getParams
    chainStateToChainIndex nid <$> use esChainState
  getChainState = do
    chainStateToContractModelChainState <$> use esChainState

-- | Sanity check a `ContractModel`. Ensures that wallet balances are not always unchanged.
propSanityCheckModel :: forall state. (ContractModel state) => QC.Property
propSanityCheckModel =
  QC.expectFailure (noBalanceChanges . stateAfter @state)
  where
    noBalanceChanges s = all symIsZero (s ^. balanceChanges)

{- | Sanity check a `ContractModel`. Ensures that all assertions in
the property generation succeed.
-}
propSanityCheckAssertions :: forall state. (ContractModel state) => Actions state -> QC.Property
propSanityCheckAssertions as = asserts $ stateAfter as

-- | Default initial assignment of value to wallet addresses
defInitialDist :: Map CardanoAddress C.Value
defInitialDist = Map.fromList $ (,Value.adaValueOf 100_000_000) <$> knownAddresses

{- | Run `Actions` in the emulator and check that the model and the emulator agree on the final
  wallet balance changes. Starts with 100.000.000 Ada for each wallet and the default parameters.
-}
propRunActions_
  :: forall state
   . (RunModel state EmulatorM)
  => Actions state
  -- ^ The actions to run
  -> Property
propRunActions_ = propRunActions (\_ _ -> Nothing) balanceChangePredicate

propRunActions
  :: forall state
   . (RunModel state EmulatorM)
  => (ModelState state -> EmulatorLogs -> Maybe String)
  -- ^ Predicate to check at the end of execution
  -> (C.LedgerProtocolParameters C.BabbageEra -> ContractModelResult state -> Property)
  -- ^ Predicate to run on the contract model
  -> Actions state
  -- ^ The actions to run
  -> Property
propRunActions =
  propRunActionsWithOptions defInitialDist def

propRunActionsWithOptions
  :: forall state
   . (RunModel state EmulatorM)
  => Map CardanoAddress C.Value
  -- ^ Initial distribution of funds
  -> E.Params
  -- ^ Node parameters
  -> (ModelState state -> EmulatorLogs -> Maybe String)
  -- ^ Predicate to check at the end of execution
  -> (C.LedgerProtocolParameters C.BabbageEra -> ContractModelResult state -> Property)
  -- ^ Predicate to run on the contract model
  -> Actions state
  -- ^ The actions to run
  -> Property
propRunActionsWithOptions initialDist params finalPred modelPred actions =
  asserts finalState
    QC..&&. monadic runFinalPredicate monadicPredicate
  where
    finalState = stateAfter actions

    monadicPredicate :: PropertyM (RunMonad EmulatorM) Property
    monadicPredicate = do
      result <- runContractModel actions
      pure $ modelPred (ledgerProtocolParameters params) result

    runFinalPredicate
      :: RunMonad EmulatorM Property
      -> Property
    runFinalPredicate contract =
      let (res, lg) =
            (\m -> evalRWS m params (emptyEmulatorStateWithInitialDist initialDist))
              . runExceptT
              . fmap fst
              . runWriterT
              . unRunMonad
              $ contract
       in monadicIO $
            let logs = Text.unpack (renderLogs lg)
             in case (res, finalPred finalState lg) of
                  (Left err, _) ->
                    return $
                      counterexample (logs ++ "\n" ++ show err) $
                        property False
                  (Right prop, Just msg) -> return $ counterexample (logs ++ "\n" ++ msg) prop
                  (Right prop, Nothing) -> return $ counterexample logs prop

balanceChangePredicate
  :: C.LedgerProtocolParameters C.BabbageEra -> ContractModelResult state -> Property
balanceChangePredicate ledgerPP result =
  let prettyAddr a = fromMaybe (show a) $ lookup (show a) prettyWalletNames
   in assertBalanceChangesMatch
        (BalanceChangeOptions False signerPaysFees ledgerPP prettyAddr)
        result

-- | Check a threat model on all transactions produced by the given actions.
checkThreatModelWithOptions
  :: (RunModel state EmulatorM)
  => Map CardanoAddress C.Value
  -- ^ Initial distribution of funds
  -> E.Params
  -- ^ Node parameters
  -> ThreatModel a
  -> Actions state
  -- ^ The actions to run
  -> Property
checkThreatModelWithOptions initialDist params =
  propRunActionsWithOptions initialDist params (\_ _ -> Nothing) . assertThreatModel

checkDoubleSatisfactionWithOptions
  :: (RunModel state EmulatorM)
  => Map CardanoAddress C.Value
  -- ^ Initial distribution of funds
  -> E.Params
  -- ^ Node parameters
  -> Actions state
  -- ^ The actions to run
  -> Property
checkDoubleSatisfactionWithOptions initialDist params =
  checkThreatModelWithOptions initialDist params doubleSatisfaction

prettyWalletNames :: [(String, String)]
prettyWalletNames = [(show addr, "Wallet " ++ show nr) | (addr, nr) <- zip knownAddresses [1 .. 10 :: Int]]

-- Note `chainStateToChainIndex` below is moved from `Plutus.Contract.Test.ContractModel.Internal`
-- and could use some serious clean up. Mostly to get rid of the conversions to/from plutus types.

-- Note, we don't store the genesis transaction in the index but put it in the before state
-- instead to avoid showing that as a balance change in the models.
chainStateToChainIndex :: CardanoAPI.NetworkId -> E.ChainState -> ChainIndex
chainStateToChainIndex nid cs =
  ChainIndex -- The Backwards order
    { transactions =
        fst $
          foldr
            addBlock
            ([], beforeState)
            ( reverse
                . drop 1
                . reverse
                . view E.chainNewestFirst
                $ cs
            )
    , networkId = nid
    }
  where
    beforeState =
      CM.ChainState
        { slot = 0
        , utxo = Index.initialise (take 1 $ reverse (cs ^. E.chainNewestFirst))
        }
    addBlock block (txs, state) =
      ( txs
          ++ [ TxInState
              ((\(CardanoEmulatorEraTx tx') -> tx') . unOnChain $ tx)
              state
              (onChainTxIsValid tx)
             | tx <- block
             ]
      , updateState block state
      )

    updateState :: [OnChainTx] -> CM.ChainState -> CM.ChainState
    updateState block state =
      CM.ChainState
        { slot = slot state + 1
        , utxo = Index.insertBlock block (utxo state)
        }

chainStateToContractModelChainState :: E.ChainState -> CM.ChainState
chainStateToContractModelChainState cst =
  ChainState
    { utxo = cst ^. E.index
    , slot = fromIntegral $ cst ^. E.chainCurrentSlot
    }

instance QCCM.HasSymbolics Builtins.BuiltinByteString where
  getAllSymbolics _ = mempty
deriving via
  QCSM.HasNoVariables Builtins.BuiltinByteString
  instance
    QCSM.HasVariables Builtins.BuiltinByteString

instance QCCM.SymValueLike Value where
  toSymValue = either (error . show) QCCM.toSymValue . Value.toCardanoValue