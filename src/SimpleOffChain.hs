{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


module SimpleOffChain where

import           Control.Monad          hiding (fmap)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.OpenApi.Schema         (ToSchema)
import qualified Data.Map                    as Map
import           Data.Maybe (fromJust)
import           Data.Void (Void)
import           Data.Text (Text)
import           GHC.Generics                (Generic)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints          as Constraints
import qualified Ledger.Constraints.TxConstraints as TxConstraints
import           Ledger.Value                as Value
import           Plutus.Contract             as Contract
import           Plutus.Contract.Wallet
import           PlutusTx.Builtins           as Builtins
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import qualified PlutusTx

import           Prelude                     (Semigroup (..), Show (..), String)
import           Text.Printf            (printf)

import qualified MintingNft
import qualified Plutus.V1.Ledger.Ada as Ada
import PlutusTx.IsData.Class (ToData(toBuiltinData))
import qualified Ledger.Constraints as Constraints
import Plutus.Contract (submitTxConstraints)
import qualified Plutus.Contract.StateMachine as Constraints
import Ledger (unitRedeemer)

type TestingSchema =
            Endpoint "lock" ()
        .\/ Endpoint "unlock" ()

lock :: AsContractError e => Contract w s e ()
lock = do
    oref <- getUnspentOutput -- Contract w s e TxOutRef
    o    <- fromJust <$> Contract.txOutFromRef oref
    pkh  <- Contract.ownPaymentPubKeyHash

    let mpolicy = MintingNft.policy oref
    let lookups = Constraints.mintingPolicy mpolicy <>
                  Constraints.unspentOutputs (Map.singleton oref o)

    let cs  = scriptCurrencySymbol mpolicy
        val = Value.singleton cs "ff" (1 :: Integer)
        sc1val = Ada.lovelaceValueOf 2000000 <> val
    logInfo @String $ printf "value to be minted %s" (show val)
    logInfo @String $ printf "contract pkh %s" (show pkh)
    logInfo @String $ printf "contract o %s" (show o)
    logInfo @String $ printf "value to be minted %s" (show val)
    logInfo @String $ printf "value to be paid to pkh %s" (show sc1val)

    let constraints = TxConstraints.mustSpendPubKeyOutput oref <>
                      TxConstraints.mustMintValueWithRedeemer unitRedeemer val <>
                      TxConstraints.mustPayToPubKey pkh sc1val

    ledgerTx <- submitTxConstraintsWith @Void lookups constraints
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "success"

endpoints :: Contract () TestingSchema Text ()
endpoints = awaitPromise proc >> endpoints
  where
    proc = lock'
    lock'   = endpoint @"lock" $ const lock
