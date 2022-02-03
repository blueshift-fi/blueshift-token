network=$1
if [ $network == "mainnet" ];
  then network_setting="--mainnet"
  else network_setting="--testnet-magic $TESTNET_MAGIC_NUM"
fi

source functions.sh
getInputTx $2
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
FROM_BALANCE=${SELECTED_UTXO_LOVELACE}

read -p 'Lovelace to send: ' LOVELACE_TO_SEND
read -p 'Receiving wallet name: ' TO_WALLET_NAME

TO_WALLET_ADDRESS=$(cat ./$network/wallets/$TO_WALLET_NAME.addr)

$CARDANO_CLI transaction build \
  --alonzo-era \
  $network_setting \
  --change-address=${FROM_WALLET_ADDRESS} \
  --tx-in ${FROM_UTXO} \
  --tx-out ${TO_WALLET_ADDRESS}+${LOVELACE_TO_SEND} \
  --out-file tx.body

$CARDANO_CLI transaction sign \
  $network_setting \
  --tx-body-file tx.body \
  --signing-key-file ./$network/wallets/${FROM_WALLET_NAME}.skey \
  --out-file tx.signed

$CARDANO_CLI transaction submit \
  $network_setting \
  --tx-file tx.signed
