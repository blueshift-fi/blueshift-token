{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Implements a custom currency with a minting policy that allows
--   the minting of a fixed amount of units.
module Blueshift.Currency.OneShotCurrency
    ( OneShotCurrency(..)
    , mkCurrency
    , oneShotCurrencyMintingPolicy
    , oneShotCurrencyMintingScriptAsCbor
    , oneShotCurrencyMintingScriptSBS
    , apiOneShotCurrencyMintingScript
    ) where

import Control.Lens
import PlutusTx.Prelude hiding (Monoid (..), Semigroup (..), unless)

import Ledger hiding (singleton)
import Ledger.Contexts qualified as V
import Ledger.Scripts
import PlutusTx qualified

import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (TokenName (..), Value)
import Ledger.Value qualified as Value

import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup (Last (..))
import GHC.Generics (Generic)
import PlutusTx.AssocMap qualified as AssocMap
import Prelude (Semigroup (..))
import Prelude qualified as Haskell

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

{- HLINT ignore "Use uncurry" -}

-- | A currency that can be created exactly once
data OneShotCurrency = OneShotCurrency
  { curRefTransactionOutput :: (TxId, Integer)
  -- ^ Transaction input that must be spent when
  --   the currency is minted.
  , curAmounts              :: AssocMap.Map TokenName Integer
  -- ^ How many units of each 'TokenName' are to
  --   be minted.
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''OneShotCurrency [('OneShotCurrency, 0)]
PlutusTx.makeLift ''OneShotCurrency


currencyValue :: CurrencySymbol -> OneShotCurrency -> Value
currencyValue s OneShotCurrency{curAmounts = amts} =
    let
        values = map (\(tn, i) -> Value.singleton s tn i) (AssocMap.toList amts)
    in fold values

mkCurrency :: TxOutRef -> [(TokenName, Integer)] -> OneShotCurrency
mkCurrency (TxOutRef h i) amts =
    OneShotCurrency
        { curRefTransactionOutput = (h, i)
        , curAmounts              = AssocMap.fromList amts
        }

{-# INLINABLE mkPolicy #-}
mkPolicy :: OneShotCurrency -> () -> V.ScriptContext -> Bool
mkPolicy c@(OneShotCurrency (refHash, refIdx) _) _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo} =
    let
        ownSymbol = V.ownCurrencySymbol ctx

        minted = V.txInfoMint txinfo
        expected = currencyValue ownSymbol c

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = expected == minted
            in traceIfFalse "C0" {-"Value minted different from expected"-} v

        -- True if the pending transaction spends the output
        -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = V.spendsOutput txinfo refHash refIdx
            in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && txOutputSpent

-- | Compiles the policy
oneShotCurrencyMintingPolicy :: OneShotCurrency -> MintingPolicy
oneShotCurrencyMintingPolicy cur = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \c -> Scripts.wrapMintingPolicy (mkPolicy c) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode cur

-- | Generates the plutus script
oneShotCurrencyMintingScript :: OneShotCurrency -> Script
oneShotCurrencyMintingScript osc = unMintingPolicyScript $ oneShotCurrencyMintingPolicy osc

-- | Generates the validator
mintingValidator :: OneShotCurrency -> Validator
mintingValidator osc = Validator $ oneShotCurrencyMintingScript osc

-- | Serializes the contract in CBOR format.
oneShotCurrencyMintingScriptAsCbor :: OneShotCurrency -> LBS.ByteString
oneShotCurrencyMintingScriptAsCbor osc = serialise $ mintingValidator osc

-- | Serializes the contract in CBOR format.
oneShotCurrencyMintingScriptSBS :: OneShotCurrency -> SBS.ShortByteString
oneShotCurrencyMintingScriptSBS osc =  SBS.toShort . LBS.toStrict $ oneShotCurrencyMintingScriptAsCbor osc

-- | Gets a serizlize plutus script from the given OneShotCurrency
apiOneShotCurrencyMintingScript :: OneShotCurrency -> PlutusScript PlutusScriptV1
apiOneShotCurrencyMintingScript osc = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ oneShotCurrencyMintingScriptAsCbor osc
