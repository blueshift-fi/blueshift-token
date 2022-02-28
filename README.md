# Minter for Cardano blockchain

Description of minting process without off-chain part (PAB or something similar).

## Prerequisite

### Binaries and environment variables

```
# native node
export CARDANO_CLI="$HOME/Job/Cardano/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.5/cardano-cli-1.32.1/x/cardano-cli/build/cardano-cli/cardano-cli"
export CARDANO_NODE_SOCKET_PATH="$HOME/Job/Cardano/liquifi-v2-cardano/pab/test-node/testnet/node.sock"

# Daedalus wallet
export CARDANO_CLI="$HOME/.daedalus/nix/store/mmfvxqxw645m13hsrr7jl0wch694z6lm-daedalus-cardano-bridge/bin/cardano-cli" # doesn't work
export CARDANO_NODE_SOCKET_PATH="$HOME/.local/share/Daedalus/testnet/cardano-node.socket"
```

Install and set up `cardano-node` and `cardano-cli`. Possible follow ways:

* Use Daedalus wallet for [mainnet](https://daedaluswallet.io/en/download/) or [testnet](https://testnets.cardano.org/en/testnets/cardano/get-started/wallet/).

    May help this [instruction](https://devslug.com/create-environment-variable-for-daedalus-wallet) for create and set environment variables (`CARDANO_NODE_SOCKET_PATH`, `CARDANO_CLI`).

* Use sources or binaries from [repo](https://github.com/input-output-hk/cardano-node) and [instruction](https://docs.cardano.org/getting-started/installing-the-cardano-node).

If you work on **testnet** you must set `TESTNET_MAGIC_NUM` (for public testnet - 1097911063). For **mainnet** it's doesn't matter.

Check that you did everything right:

```
# mainnet
$CARDANO_CLI query tip --mainnet

# testnet
$CARDANO_CLI query tip --testnet-magic $TESTNET_MAGIC_NUM


# you can see something like this
>>>
{
    "epoch": 60,
    "hash": "eb9453a91760928b286ea5137d6f9325f89f78b9c643f1e789c63c74b1934fa3",
    "slot": 431693,
    "block": 21187,
    "era": "Mary",
    "syncProgress": "19.01"
}
```

### Cabal and GHC

Using [instruction](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/install.md/) install cabal and GHC for Haskell. It will be usfull for contract compilation.

If you will face with compilation problem then install also libsodium library using same [instruction](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/install.md/).

## Minting process

Don't forget to set variables: `CARDANO_NODE_SOCKET_PATH`, `CARDANO_CLI` and optional  `TESTNET_MAGIC_NUM`.

`<network>` can be `mainnet` or `testnet`.

Complete follow steps:

1. Prepare wallets:

    Create minter wallet (skey, pkey and address):

    ```
    ./createWallet.sh <network> minter
    ```

    Create owner wallet with Daedalus (only address). Create \<network>/wallets/owner.addr with some address from recieve panel:

    ![](Daedalus.png)

2. Get Ada:

    - From Daedalus wallet:

        Just send some Ada (tAda) to **minter** wallet using address from `minter.addr`.

    - From other wallet:

        ````
        ./sendFromWallet <network> <from-wallet>

        >>>
        # Follow instruction
        ````

    - From [faucet](https://testnets.cardano.org/en/testnets/cardano/tools/faucet/) only for public testnet.

        Send tAda to **owner** wallet using address from `owner.addr`

    You can check balance:

    ```
    ./balance.sh <network> <wallet_name>

    >>>
                                TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    18ffbd400a887ae32c04be5ca52cde31759284c4ec60f4e893cf7fbdde920232     2        5000000 lovelace + TxOutDatumNone
    ```

3. Compile minting contract

    Build `one-shot-currency` app:

    ```
    cd plutus-sources
    cabal update
    cabal build one-shot-currency
    ```

    Then compile your uniq contract setting UTxO (\<TxHash>#\<TxIx>), token name and mint amount:

    ```
    cabal run one-shot-currency "<TxHash>#<TxIx>" "BLUES" 100000000000000
    ```

4. Mint tokens

    `<minting-script>` can be:
      - anyone-can-mint;
      - one-shot-currency;
      - write own policy and compile it.

    `<amount>` will be multiplied by 10^6.

    ```
    ./mint.sh <network> one-shot-currency "BLUES" 100000000 minter owner

    # choose utxo that contain enough lovelace (~4000000) and satisfies redeemer condition
    >>>
                                TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    18ffbd400a887ae32c04be5ca52cde31759284c4ec60f4e893cf7fbdde920232     2        5000000 lovelace + TxOutDatumNone
    TX row number: 1
    Estimated transaction fee: Lovelace 364907
    Transaction successfully submitted.
    ```

    After some times check balance and look on new tokens:

    ```
    ./balance.sh <network> <wallet_name>

    >>>
                                TxHash                                 TxIx        Amount
    --------------------------------------------------------------------------------------
    a658baf0eff019433c1c5b2d3384248bc4edc3f068e888920f44854d1efffec7     0        1635093 lovelace + TxOutDatumNone
    a658baf0eff019433c1c5b2d3384248bc4edc3f068e888920f44854d1efffec7     1        3000000 lovelace + 2000000 fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50.744c5146 + TxOutDatumNone
    ```

5. Token registration (optional)

    After minting your new tokens will be usefull to register it:

    * Install `offchain-metadata-tool` from [here](https://github.com/input-output-hk/offchain-metadata-tools) and in root folder of this project.

    * Create register information and sign it by `<wallet>` (need private key)

        ```
        ./registerToken.sh testnet one-shot-currency minter \
            "BLUES" \
            "A currency for the Blueshift DEX." \
            "BLUES" \
            "https://blueshift.fi/" \
            icon.png \
            6
        ```

        `<subject>`.json will be created.
    
    * Make git pull request using this [instruction](https://github.com/cardano-foundation/cardano-token-registry/wiki/How-to-submit-an-entry-to-the-registry) and await confirmation.

    Done! More information about registration you can find [here](https://github.com/cardano-foundation/cardano-token-registry/wiki).

### Possible problems

Maybe you send (tx-in) or recieve (tx-out) not enough or wrong amount. Change some shell scripts.

## Useful references

- https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/cardano-node-cli-reference.md

- https://developers.cardano.org/docs/native-tokens/minting
