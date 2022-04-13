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

module MintingNft where

import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as Scripts
import           Ledger.Value             as Value
import qualified PlutusTx
import           PlutusTx.Prelude         hiding (Semigroup (..), unless)

{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> Integer -> ScriptContext -> Bool
mkPolicy utxo _ ctx = validate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs info

    validate :: Bool
    validate = case flattenValue $ txInfoMint info of
       [(_, _, amt)] -> (amt == 1 && hasUTxO) || amt == (-1)
       _             -> False

policy :: TxOutRef -> Scripts.MintingPolicy
policy utxo = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode utxo

plutusScript :: TxOutRef -> Script
plutusScript = unMintingPolicyScript . policy

validator :: TxOutRef ->  Validator
validator = Validator . plutusScript

scriptAsCbor :: TxOutRef ->  LB.ByteString
scriptAsCbor = serialise . validator

mintingNft :: TxOutRef ->  PlutusScript PlutusScriptV1
mintingNft = PlutusScriptSerialised . SBS.toShort . LB.toStrict . scriptAsCbor

mintingNftShortBs :: TxOutRef ->  SBS.ShortByteString
mintingNftShortBs = SBS.toShort . LB.toStrict . scriptAsCbor
