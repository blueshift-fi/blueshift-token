#!/bin/bash

network=$1
if [ $network == "mainnet" ];
  then network_setting="--mainnet"
  else network_setting="--testnet-magic $TESTNET_MAGIC_NUM"
fi

wallet_name=$2

$CARDANO_CLI query utxo \
  $network_setting \
  --address $(cat ./$network/wallets/$wallet_name.addr)
