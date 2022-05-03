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
module LockingContract
  ( lockingContractScript
  , lockingContractScriptShortBs
  , contract
  , CustomDatumType
  , Schema
  ) where
import           Codec.Serialise
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import           Ledger                   hiding (singleton)
import qualified Ledger.Typed.Scripts     as TypedScripts
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Ada     as Ada
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Plutus.V1.Ledger.Value   as Value
import           CheckFuncs
import           DataTypes
-- import           Data.Maybe
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0

    @see remove

  Versions Used In Project:

  cardano-cli 1.34.1 - linux-x86_64 - ghc-8.10
  git rev 73f9a746362695dc2cb63ba757fbcabb81733d23

  cabal-install version 3.4.0.0
  compiled using version 3.4.0.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.4
-}
-------------------------------------------------------------------------------
-- | Create the token sale parameters data object.
-------------------------------------------------------------------------------
data LockingContractParams = LockingContractParams {}
PlutusTx.makeLift ''LockingContractParams
-------------------------------------------------------------------------------
-- | Create the redeemer parameters data object.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove OutboundRedeemerType | 
                          Add InboundRedeemerType     |
                          Close
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Remove, 0)
                                                , ('Add,    1) 
                                                , ('Close,  2) 
                                                ]
PlutusTx.makeLift ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | mkValidator :: Data -> Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: LockingContractParams -> CustomDatumType -> CustomRedeemerType -> ScriptContext -> Bool
mkValidator _ datum redeemer context =
  case redeemer of
    (Remove ort) -> do
      { let a = traceIfFalse "MultiSig Failure"       $ txSignedBy info (ortSellerPKH ort) && txSignedBy info profitPKH
      ; let b = traceIfFalse "Script UTxO Failure"    $ isNScriptInputs txInputs (1 :: Integer)
      ; let c = traceIfFalse "Cont Value Failure"     $ isValueContinuing contOutputs (validatingValue - (Ada.lovelaceValueOf $ ortAmount ort))
      ; let d = traceIfFalse "User Value Failure"     $ isPKHGettingValue txOutputs (ortSellerPKH ort) (Ada.lovelaceValueOf $ ortAmount ort)
      ; let e = traceIfFalse "Profit Value Failure"   $ isPKHGettingValue txOutputs profitPKH (Ada.lovelaceValueOf $ ortProfit ort)
      ; let f = traceIfFalse "Value is at Minimum"    $ Value.gt validatingValue minimumValue
      ; let g = traceIfFalse "Incoming Datum Failure" $ isEmbeddedDatum datum info contOutputs
      ;         traceIfFalse "Error: Remove Endpoint" $ all (==True) [a,b,c,d,e,f,g]
      }
    (Add irt) -> do
      { let a = traceIfFalse "User Signature Failure" $ txSignedBy info (irtSellerPKH irt)
      ; let b = traceIfFalse "Script UTxO Failure"    $ isNScriptInputs txInputs (1 :: Integer)
      ; let c = traceIfFalse "Cont Value Failure"     $ isValueContinuing contOutputs (validatingValue + (Ada.lovelaceValueOf $ irtAmount irt))
      ; let d = traceIfFalse "Incoming Datum Failure" $ isEmbeddedDatum datum info contOutputs
      ;         traceIfFalse "Error: Add Endpoint"    $ all (==True) [a,b,c,d]
      }
    Close -> do
      { let a = traceIfFalse "Signature Failure"      $ txSignedBy info profitPKH
      ; let b = traceIfFalse "Profit Value Failure"   $ isPKHGettingValue txOutputs profitPKH minimumValue
      ; let c = traceIfFalse "Value is at Minimum"    $ validatingValue == minimumValue
      ;         traceIfFalse "Error: Remove Endpoint" $ all (==True) [a,b,c]
      }
  where
    info :: TxInfo
    info = scriptContextTxInfo context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs info

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    contOutputs :: [TxOut]
    contOutputs = getContinuingOutputs context

    profitPKH :: PubKeyHash
    profitPKH = cdtProfitPKH datum

    validatingValue :: Value
    validatingValue =
      case findOwnInput context of
        Nothing    -> traceError "ERROR!"
        Just input -> txOutValue $ txInInfoResolved input

    minimumValue :: Value
    minimumValue = Ada.lovelaceValueOf (5000000 :: Integer)
-------------------------------------------------------------------------------
-- | This determines the data type for Datum and Redeemer.
-------------------------------------------------------------------------------
data Typed
instance TypedScripts.ValidatorTypes Typed where
  type instance DatumType    Typed = CustomDatumType
  type instance RedeemerType Typed = CustomRedeemerType
-------------------------------------------------------------------------------
-- | Now we need to compile the Typed Validator.
-------------------------------------------------------------------------------
typedValidator :: LockingContractParams -> TypedScripts.TypedValidator Typed
typedValidator sp = TypedScripts.mkTypedValidator @Typed
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode sp)
   $$(PlutusTx.compile [|| wrap        ||])
    where
      wrap = TypedScripts.wrapValidator @CustomDatumType @CustomRedeemerType  -- @Datum @Redeemer
-------------------------------------------------------------------------------
-- | Define The Token Sale Parameters Here
-------------------------------------------------------------------------------
validator :: Scripts.Validator
validator = TypedScripts.validatorScript (typedValidator $ LockingContractParams {})
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator

lockingContractScriptShortBs :: SBS.ShortByteString
lockingContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

lockingContractScript :: PlutusScript PlutusScriptV1
lockingContractScript = PlutusScriptSerialised lockingContractScriptShortBs
-------------------------------------------------------------------------------
-- | Off Chain
-------------------------------------------------------------------------------
type Schema = Endpoint "" ()
contract :: AsContractError e => Contract () Schema e ()
contract = selectList [] >> contract