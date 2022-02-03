#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -u
set -o pipefail

network=$1
if [ $network == "mainnet" ];
  then network_setting="--mainnet"
  else network_setting="--testnet-magic $TESTNET_MAGIC_NUM"
fi

source functions.sh

POLICY_NAME=$2
echo "Mintintg policy: ${POLICY_NAME}"

COIN_NAME=$(echo -n $3 | xxd -ps)
let TOKEN_COUNT=$4*1000000

MINT_WALLET_NAME=$5
OWNER_WALLET_NAME=$6
OWNER_WALLET_ADDR=$(cat ./$network/wallets/$OWNER_WALLET_NAME.addr)

$CARDANO_CLI query protocol-parameters \
  $network_setting \
  --out-file $network/pparams.json

POLICY_ID=$($CARDANO_CLI transaction policyid --script-file ./scripts/$POLICY_NAME.plutus)

echo "Policy ID: ${POLICY_ID}"
echo "Token name: ${COIN_NAME}"
echo "Amount: ${TOKEN_COUNT}"
echo "Owner: ${OWNER_WALLET_NAME}"


getInputTx $MINT_WALLET_NAME
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}

echo "Build"
$CARDANO_CLI transaction build \
  --alonzo-era \
  --cardano-mode \
  $network_setting \
  --change-address=${OWNER_WALLET_ADDR} \
  --tx-in ${FROM_UTXO} \
  --tx-in-collateral ${FROM_UTXO} \
  --mint-script-file="./scripts/$POLICY_NAME.plutus" \
  --mint-redeemer-file "./scripts/$POLICY_NAME.redeemer" \
  --mint="${TOKEN_COUNT} ${POLICY_ID}.${COIN_NAME}" \
  --tx-out "${OWNER_WALLET_ADDR} + 2000000 + ${TOKEN_COUNT} ${POLICY_ID}.${COIN_NAME}" \
  --protocol-params-file "$network/pparams.json" \
  --out-file "./tmp/mint.build" \

echo "Sign"
$CARDANO_CLI transaction sign \
  $network_setting \
  --tx-body-file "./tmp/mint.build" \
  --signing-key-file ./$network/wallets/${FROM_WALLET_NAME}.skey \
  --out-file "./tmp/mint.signed"

echo "Submit"
$CARDANO_CLI transaction submit \
  $network_setting \
  --tx-file "./tmp/mint.signed"
