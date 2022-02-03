#!/usr/bin/env bash

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -e
set -u
set -o pipefail

network=$1

source functions.sh

POLICY_NAME=$2
echo "Mintintg policy: ${POLICY_NAME}"

OWNER_WALLET_NAME=$3
OWNER_WALLET_ADDR=$(cat ./$network/wallets/$OWNER_WALLET_NAME.addr)
echo "Register: ${OWNER_WALLET_NAME}"

COIN_NAME=$4
echo "Token name: ${COIN_NAME}"

description=$5
ticker=$6
url=$7
logo=$8
decimals=$9


POLICY_ID=$($CARDANO_CLI transaction policyid --script-file ./scripts/$POLICY_NAME.plutus)
COIN_NAME_CODE=$(echo -n ${COIN_NAME} | xxd -ps)

echo "Policy ID: ${POLICY_ID}"
echo "Token name code: ${COIN_NAME_CODE}"


SUBJECT="${POLICY_ID}${COIN_NAME_CODE}"
echo "Subject: ${SUBJECT}"

./token-metadata-creator entry --init $SUBJECT

./token-metadata-creator entry $SUBJECT \
  --name "${COIN_NAME}" \
  --description "${description}"

./token-metadata-creator entry $SUBJECT \
  --ticker $ticker \
  --url $url \
  --logo $logo \
  --decimals $decimals

./token-metadata-creator entry ${SUBJECT} -a ./${network}/wallets/${OWNER_WALLET_NAME}.skey

./token-metadata-creator entry ${SUBJECT} --finalize
