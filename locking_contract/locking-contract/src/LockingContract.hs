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
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  Version  : Rev 0
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
                          Add     InboundRedeemerType |
                          Close                       |
                          Vote
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ('Remove, 0)
                                                , ('Add,    1)
                                                , ('Close,  2)
                                                , ('Vote,   3)
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
      { let outboundADA = Ada.lovelaceValueOf $ ortAmount ort
      ; let userPKH     = ortSellerPKH ort
      ; let a = traceIfFalse "MultiSig Failure"       $ txSignedBy info userPKH && hasEnoughSigners info datum majorityParam
      ; let b = traceIfFalse "Script UTxO Failure"    $ isNScriptInputs txInputs (1 :: Integer)
      ; let c = traceIfFalse "Cont Value Failure"     $ isValueContinuing contOutputs (validatingValue - outboundADA)
      ; let d = traceIfFalse "User Value Failure"     $ isPKHGettingValue txOutputs userPKH outboundADA
      ; let e = traceIfFalse "Value Is At Minimum"    $ Value.geq validatingValue (minimumValue + outboundADA)
      ; let f = traceIfFalse "Incoming Datum Failure" isDatumCorrect
      ;         traceIfFalse "Error: Remove Endpoint" $ all (==True) [a,b,c,d,e,f]
      }
    (Add irt) -> do
      { let outboundADA = Ada.lovelaceValueOf $ irtAmount irt
      ; let userPKH     = irtSellerPKH irt
      ; let a = traceIfFalse "User Signature Failure" $ txSignedBy info userPKH
      ; let b = traceIfFalse "Script UTxO Failure"    $ isNScriptInputs txInputs (1 :: Integer)
      ; let c = traceIfFalse "Cont Value Failure"     $ isValueContinuing contOutputs (validatingValue + outboundADA)
      ; let d = traceIfFalse "Incoming Datum Failure" isDatumCorrect
      ;         traceIfFalse "Error: Add Endpoint"    $ all (==True) [a,b,c,d]
      }
    Close -> do
      { let a = traceIfFalse "Signature Failure"       $ hasEnoughSigners info datum majorityParam
      ; let b = traceIfFalse "Value Is Not At Minimum" $ validatingValue == minimumValue
      ; let c = traceIfFalse "Script UTxO Failure"     $ isNScriptInputs txInputs (1 :: Integer)
      ;         traceIfFalse "Error: Close Endpoint"   $ all (==True) [a,b,c]
      }
    Vote -> do
      { let a = traceIfFalse "Signature Failure"      $ hasEnoughSigners info datum majorityParam
      ; let b = traceIfFalse "Cont Value Failure"     $ isValueContinuing contOutputs validatingValue
      ; let c = traceIfFalse "Incoming Datum Failure" $ isVoteOccurring || isMemberAdded || isMemberRemoved
      ; let d = traceIfFalse "Script UTxO Failure"    $ isNScriptInputs txInputs (1 :: Integer)
      ;         traceIfFalse "Error: Vote Endpoint"   $ all (==True) [a,b,c,d]
      }
  where
    isDatumCorrect :: Bool
    isDatumCorrect = 
      case isEmbeddedDatum datum info contOutputs of
        Nothing       -> False
        Just embedded -> datum == embedded
    
    isMemberAdded :: Bool
    isMemberAdded = 
      case isEmbeddedDatum datum info contOutputs of
        Nothing       -> False
        Just embedded -> datum =+= embedded
    
    isMemberRemoved :: Bool
    isMemberRemoved = 
      case isEmbeddedDatum datum info contOutputs of
        Nothing       -> False
        Just embedded -> datum =-= embedded
    
    isVoteOccurring :: Bool
    isVoteOccurring = 
      case isEmbeddedDatum datum info contOutputs of
        Nothing       -> False
        Just embedded -> datum =?= embedded
    
    info :: TxInfo
    info = scriptContextTxInfo context

    txInputs :: [TxInInfo]
    txInputs = txInfoInputs info

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    contOutputs :: [TxOut]
    contOutputs = getContinuingOutputs context

    majorityParam :: Integer
    majorityParam = cdtMajority datum

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