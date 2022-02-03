#!/bin/bash

network=$1
if [ $network == "mainnet" ];
  then network_setting="--mainnet"
  else network_setting="--testnet-magic $TESTNET_MAGIC_NUM"
fi

wallet_name=$2

cd $network/wallets

$CARDANO_CLI address key-gen \
  --verification-key-file $wallet_name.vkey \
  --signing-key-file $wallet_name.skey

$CARDANO_CLI address build \
  $network_setting \
  --payment-verification-key-file $wallet_name.vkey \
  --out-file $wallet_name.addr
