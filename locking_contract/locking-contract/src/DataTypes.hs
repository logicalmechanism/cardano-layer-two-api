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
  , cdtMajority
  , OutboundRedeemerType
  , ortSellerPKH
  , ortAmount
  , ortProfitPKH
  , ortProfit
  , ortProof
  , InboundRedeemerType
  , irtSellerPKH
  , irtAmount
  , (=+=)
  , (=-=)
  , (=?=)
  , length'
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger                    hiding (singleton)
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
-- | Define custom equality class called Equiv.
-------------------------------------------------------------------------------
class Equiv a b where
  (=+=) :: a -> b -> Bool
  (=-=) :: a -> b -> Bool
  (=?=) :: a -> b -> Bool
-------------------------------------------------------------------------------
-- | Outbound data ( user wants to leave )
-------------------------------------------------------------------------------
data OutboundRedeemerType = OutboundRedeemerType
  { ortSellerPKH :: !PubKeyHash
  , ortAmount    :: !Integer
  , ortProfitPKH :: !PubKeyHash
  , ortProfit    :: !Integer
  , ortProof     :: !BuiltinByteString
  }
    deriving stock    (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''OutboundRedeemerType
PlutusTx.makeLift           ''OutboundRedeemerType
-------------------------------------------------------------------------------
-- | Inbound data ( user wants to enter )
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
  { cdtProfitPKH :: ![PubKeyHash]
  , cdtMajority  :: !Integer
  }
    deriving stock    (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.unstableMakeIsData ''CustomDatumType
PlutusTx.makeLift           ''CustomDatumType
-- old is a; new is b
instance Eq CustomDatumType where
  {-# INLINABLE (==) #-}
  a == b = ( cdtProfitPKH a == cdtProfitPKH b ) &&
           ( cdtMajority  a == cdtMajority  b )
-------------------------------------------------------------------------------
-- | Create equiv instance
-------------------------------------------------------------------------------
length' ::  [PubKeyHash] -> Integer
length' pkhs = length'' pkhs 0
  where
    length'' :: [PubKeyHash] -> Integer -> Integer
    length'' [] counter = counter
    length'' (_:pkhs') !counter = length'' pkhs' (counter + 1)
-- old is a; new is b
instance Equiv CustomDatumType CustomDatumType where
  {-# INLINABLE (=+=) #-}
  a =+= b = ( length' (cdtProfitPKH a) == length' (cdtProfitPKH b) + 1 ) &&
            ( cdtMajority  a == cdtMajority  b )
  {-# INLINABLE (=-=) #-}
  a =-= b = ( length' (cdtProfitPKH a) == length' (cdtProfitPKH b) - 1 ) &&
            ( cdtMajority  a == cdtMajority  b )
  {-# INLINABLE (=?=) #-}
  a =?= b = ( cdtProfitPKH a == cdtProfitPKH b ) &&
            ( cdtMajority  a /= cdtMajority  b )