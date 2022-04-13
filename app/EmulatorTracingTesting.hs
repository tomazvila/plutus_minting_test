{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Map                   as Map
import           Control.Monad              hiding (fmap)
import           Plutus.Contract            as Contract hiding (waitNSlots)
import           Plutus.V1.Ledger.Ada
import           Plutus.Trace.Emulator as Emulator
    ( initialChainState, activateContractWallet, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, (<>))

import           Wallet.Emulator.Wallet
import           Ledger.Value
import           Data.Default (def, Default (..))
import           Control.Lens

import           SimpleOffChain

main :: IO ()
main = runEmulatorTraceIO' def emCfg $ do
          let w1 = knownWallet 1
          w1 <- activateContractWallet w1 endpoints
          callEndpoint @"lock" w1 ()
          void $ waitNSlots 2

collateralValue :: Value
collateralValue = singleton "ff" "CONY" 100

loanValue :: Value
loanValue = singleton "ff" "CONYMONY" 150

interestValue :: Value
interestValue = singleton "ff" "MONY" 50

emCfg :: EmulatorConfig
emCfg = def & initialChainState .~ Left (Map.fromList xs)
    where
        xs = [(knownWallet 1, ada1), (knownWallet 2, ada2)]
        ada1 = lovelaceValueOf 100_000_000 <> collateralValue <> interestValue
        ada2 = lovelaceValueOf 100_000_000 <> loanValue
