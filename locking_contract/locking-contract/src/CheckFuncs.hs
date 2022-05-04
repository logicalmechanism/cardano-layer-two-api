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
  , isPKHGettingValue
  , hasEnoughSigners
  ) where
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Address as Address
import           Ledger                   hiding (singleton)
import           DataTypes                (CustomDatumType (cdtProfitPKH), length')
import qualified PlutusTx
{-
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
-}
-------------------------------------------------------------------------------
-- | Search each TxOut for the correct address and value.
-------------------------------------------------------------------------------
isPKHGettingValue :: [TxOut] -> PubKeyHash -> Value -> Bool
isPKHGettingValue [] _pkh _val = False
isPKHGettingValue (x:xs) pkh val
  | checkAddr && checkVal = True
  | otherwise             = isPKHGettingValue xs pkh val
  where
    checkAddr :: Bool
    checkAddr = txOutAddress x == Address.pubKeyHashAddress pkh

    checkVal :: Bool
    checkVal = txOutValue x == val
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
isEmbeddedDatum :: CustomDatumType -> TxInfo -> [TxOut] -> Maybe CustomDatumType
isEmbeddedDatum _     _    []     = Nothing
isEmbeddedDatum datum info (x:xs) =
  case txOutDatumHash x of
    Nothing -> isEmbeddedDatum datum info xs
    Just dh ->
      case findDatum dh info of
        Nothing        -> Nothing
        Just (Datum d) -> PlutusTx.fromBuiltinData d
-------------------------------------------------------------------------------
-- | Count the number of signers that are from the group
-------------------------------------------------------------------------------
hasEnoughSigners :: TxInfo -> CustomDatumType -> Integer -> Bool
hasEnoughSigners info datum majorityParam = txWeight >= majority
  where
    txWeight :: Integer
    txWeight = calculateWeight txSigners nodeGroup 0

    txSigners :: [PubKeyHash]
    txSigners = txInfoSignatories info

    nodeGroup :: [PubKeyHash]
    nodeGroup = cdtProfitPKH datum

    majority :: Integer
    majority = if amount < majorityParam then amount else majorityParam
      where
        amount :: Integer
        amount = length' nodeGroup

    calculateWeight :: [PubKeyHash] -> [PubKeyHash] -> Integer -> Integer
    calculateWeight []               _             counter = counter
    calculateWeight (signer:signers) signingGroup  counter
      | checkSigneeInGroup signer signingGroup = calculateWeight signers signingGroup (counter + 1)
      | otherwise                              = calculateWeight signers signingGroup counter
        where
          checkSigneeInGroup :: PubKeyHash -> [PubKeyHash] -> Bool
          checkSigneeInGroup _ [] = False
          checkSigneeInGroup pkh (vestor:vestors)
            | pkh == vestor = True
            | otherwise     = checkSigneeInGroup pkh vestors
