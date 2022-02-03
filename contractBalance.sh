#!/bin/bash

network=$1
if [ $network == "mainnet" ];
  then network_setting="--mainnet"
  else network_setting="--testnet-magic $TESTNET_MAGIC_NUM"
fi

SCRIPT_NAME=$2
SCRIPT_ADDRESS=$($CARDANO_CLI address build $network_setting --payment-script-file ./scripts/${SCRIPT_NAME}.plutus)
echo $SCRIPT_ADDRESS > ./$network/wallets/${SCRIPT_NAME}.addr
./balance.sh $network ${SCRIPT_NAME}
