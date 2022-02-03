function getInputTx() {
	BALANCE_FILE=/tmp/walletBalances.txt
	rm -f $BALANCE_FILE
	if [ -z "$1" ]
	then
		read -p 'Wallet Name: ' SELECTED_WALLET_NAME
	else
		echo 'Wallet Name: ' $1
		SELECTED_WALLET_NAME=$1
	fi
	./balance.sh $network $SELECTED_WALLET_NAME > $BALANCE_FILE
	SELECTED_WALLET_ADDR=$(cat ./$network/wallets/$SELECTED_WALLET_NAME.addr)
	cat $BALANCE_FILE
	read -p 'TX row number: ' TMP
	TX_ROW_NUM="$(($TMP+2))"
	TX_ROW=$(sed "${TX_ROW_NUM}q;d" $BALANCE_FILE)
	SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
	SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
	SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
}

walletAddress() {
	WALLET_ADDRESS=$(cat ./$network/wallets/$1.addr)
}

setDatumHash() {
	DATUM_HASH=$($CARDANO_CLI transaction hash-script-data --script-data-value $DATUM_VALUE)
	#return $($CARDANO_CLI transaction hash-script-data --script-data-value $1)
}

getScriptAddress() {
	SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file ./scripts/$1.plutus $testnet_setings)
        echo $SCRIPT_ADDRESS > ./$network/wallets/$1.addr
}

function section {
  echo "============================================================================================"
  echo $1
  echo "============================================================================================"
}

function removeTxFiles() {
  rm -f tx.raw
  rm -f tx.signed
}

