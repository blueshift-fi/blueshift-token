#!/bin/bash

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
# section "From Wallet:"
getInputTx $2
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
FROM_BALANCE=${SELECTED_UTXO_LOVELACE}

read -p 'Token Amount to send: ' TOKEN_AMOUNT_TO_SEND
read -p 'Token Name: ' TOKEN_NAME
read -p 'Receiving wallet name: ' TO_WALLET_NAME

TO_WALLET_ADDRESS=$(cat ./$network/wallets/$TO_WALLET_NAME.addr)

let TOKEN_CHANGE=$SELECTED_UTXO_TOKENS-$TOKEN_AMOUNT_TO_SEND

$CARDANO_CLI transaction build \
  --alonzo-era \
  --tx-in ${FROM_UTXO} \
  --tx-out "${TO_WALLET_ADDRESS} + 1500000 + ${TOKEN_AMOUNT_TO_SEND} $TOKEN_NAME" \
  --tx-out "${FROM_WALLET_ADDRESS} + 1500000 + ${TOKEN_CHANGE} $TOKEN_NAME" \
  --change-address=${FROM_WALLET_ADDRESS} \
  --testnet-magic ${TESTNET_MAGIC_NUM} \
  --out-file "./tmp/tx.build" \

$CARDANO_CLI transaction sign \
  $network_setting \
  --tx-body-file "./tmp/tx.build" \
  --signing-key-file ./$network/wallets/${FROM_WALLET_NAME}.skey \
  --out-file "./tmp/tx.signed"

$CARDANO_CLI transaction submit \
  $network_setting \
  --tx-file "./tmp/tx.signed"
