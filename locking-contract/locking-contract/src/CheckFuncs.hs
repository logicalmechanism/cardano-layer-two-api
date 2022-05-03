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
module CheckFuncs
  ( isNScriptInputs
  , isValueContinuing
  , isEmbeddedDatum
  ) where
import           PlutusTx.Prelude
-- import qualified Plutus.V1.Ledger.Address as Address
-- import qualified Plutus.V1.Ledger.Value   as Value
import           Ledger                   hiding (singleton)
import           DataTypes                (CustomDatumType)
import qualified PlutusTx
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
-}
-------------------------------------------------------------------------------
-- | Search each TxOut for a value.
-------------------------------------------------------------------------------
isValueContinuing :: [TxOut] -> Value -> Bool
isValueContinuing [] _ = False
isValueContinuing (x:xs) val
  | checkVal  = True
  | otherwise = isValueContinuing xs val
  where
    checkVal :: Bool
    checkVal = txOutValue x == val
-------------------------------------------------------------------------
-- | Check if N script inputs are being used in the transaction.
-------------------------------------------------------------------------
isNScriptInputs :: [TxInInfo] -> Integer -> Bool
isNScriptInputs txInputs = loopInputs txInputs 0
  where
    loopInputs :: [TxInInfo] -> Integer -> Integer -> Bool
    loopInputs []      counter nMatch = counter == nMatch
    loopInputs (x:xs) !counter nMatch =
      case txOutDatumHash $ txInInfoResolved x of
        Nothing -> do counter <= nMatch && loopInputs xs counter nMatch
        Just _  -> do counter <= nMatch && loopInputs xs (counter + 1) nMatch
-------------------------------------------------------------------------------
-- | Check if the incoming datum is the correct form.
-------------------------------------------------------------------------------
isEmbeddedDatum :: CustomDatumType -> TxInfo -> [TxOut] -> Bool
isEmbeddedDatum _     _    []     = False
isEmbeddedDatum datum info (x:xs) = 
  case txOutDatumHash x of
    Nothing -> isEmbeddedDatum datum info xs
    Just dh -> 
      case findDatum dh info of
        Nothing        -> False
        Just (Datum d) -> 
          case PlutusTx.fromBuiltinData d of
            Nothing       -> False
            Just embedded -> datum == embedded