
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import           Prelude
import           System.Environment         ( getArgs )

import           Cardano.Api                ( writeFileTextEnvelope, displayError, scriptDataToJson, ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import           Cardano.Api.Shelley        ( fromPlutusData, PlutusScript, PlutusScriptV1 )

-- import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api       as Plutus

import qualified PlutusTx
import qualified PlutusTx.AssocMap          as AssocMap

import qualified Data.Aeson                 as Json ( encode )
import           Data.String                ( fromString )
import qualified Data.ByteString.Short      as SBS
import qualified Data.ByteString.Lazy       as LBS

import           BlueShift.Currency.OneShotCurrency



-- Change for yourself
oneShotCurrency :: OneShotCurrency
oneShotCurrency = mkCurrency
  (uncurry Plutus.TxOutRef $ parseUTxO "7a814fe915d44dc0bfac8070b77847733686eb9ad5d6afec9cdfc42f84e2a787#0")
  [(Plutus.TokenName "BlueShift token", 100_000_000 * (10^6))]

-- | Gets UTxO, TokenName, Amount, FilePath
main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let (refHash, refIdx) = if nargs > 0
                            then parseUTxO (head args)
                            else curRefTransactionOutput oneShotCurrency
  let currency =  if nargs > 2
                  then [(Plutus.TokenName $ fromString $ args!!1, read $ args!!2)]
                  else AssocMap.toList $ curAmounts oneShotCurrency
  let filepath =  if nargs > 3
                  then args!!3
                  else "./../scripts/"
  let osc = mkCurrency (Plutus.TxOutRef refHash refIdx) currency

  let scriptname = "one-shot-currency"

  -- Write plutus-script
  writePlutusScript (filepath ++ scriptname ++ ".plutus") $ apiOneShotCurrencyMintingScript osc
  evaluatePlutusScript $ oneShotCurrencyMintingScriptSBS osc

  -- Write redeemer
  print osc
  writeData (filepath ++ scriptname ++ ".redeemer") osc


-- | Parse the UTXO from its hexadecimal string representation to and TxOutRef.
parseUTxO :: String -> (Plutus.TxId, Integer)
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    (Plutus.TxId $ Plutus.getLedgerBytes $ fromString x, read $ tail y)


-- | Writes plutus script
writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> IO ()
writePlutusScript file script = do
  result <- writeFileTextEnvelope file Nothing script
  case result of
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Wrote script to file " ++ file

-- | Displays the execution budget
evaluatePlutusScript :: SBS.ShortByteString -> IO ()
evaluatePlutusScript scriptSBS = do
  case Plutus.defaultCostModelParams of
    Just m ->
      let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
      in do print ("Log output" :: String) >> print logout
            case e of
              Left evalErr -> print ("Eval Error" :: String) >> print evalErr
              Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
    Nothing -> error "defaultCostModelParams failed"

-- | Constructs the JSON file for the datum or redeemer
writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = do
  LBS.writeFile file (toJsonString isData)
  putStrLn $ "Wrote data to file " ++ file


toJsonString :: PlutusTx.ToData a => a -> LBS.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
