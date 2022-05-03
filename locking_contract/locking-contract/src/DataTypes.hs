{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module DataTypes
  ( CustomDatumType
  , cdtProfitPKH
  , OutboundRedeemerType
  , ortSellerPKH
  , ortAmount
  , ortProfit
  , InboundRedeemerType
  , irtSellerPKH
  , irtAmount
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger                    hiding (singleton)
-- import qualified Plutus.V1.Ledger.Contexts as Contexts
import           Prelude                   (Show)
import           Data.Aeson                (FromJSON, ToJSON)
import           Data.OpenApi.Schema       (ToSchema)
import           GHC.Generics              (Generic)
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
-}
-------------------------------------------------------------------------------
-- | Custom data object for storing swap half data.
-------------------------------------------------------------------------------
data OutboundRedeemerType = OutboundRedeemerType
  { ortSellerPKH :: !PubKeyHash
  , ortAmount    :: !Integer
  , ortProfit    :: !Integer
  }
    deriving stock    (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''OutboundRedeemerType
PlutusTx.makeLift           ''OutboundRedeemerType
-------------------------------------------------------------------------------
-- | Custom data object for storing swap half data.
-------------------------------------------------------------------------------
data InboundRedeemerType = InboundRedeemerType
  { irtSellerPKH :: !PubKeyHash 
  , irtAmount    :: !Integer
  }
    deriving stock    (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''InboundRedeemerType
PlutusTx.makeLift           ''InboundRedeemerType
-------------------------------------------------------------------------------
-- | Custom datum object for storing utxo data.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { cdtProfitPKH :: !PubKeyHash }
    deriving stock    (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift           ''CustomDatumType
-- old is a; new is b
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b =  (cdtProfitPKH a == cdtProfitPKH  b)