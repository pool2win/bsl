
## Introduction

Bitcoin Scripting Language (BSL) is useful for writing bitcoin
contracts that can be tested with a simulated blockchain. Both bitcoin
contracts and the blockchain are controlled by BSL.

BSL enables writing contracts that use high level concepts like
bitcoin standard scripts and tweaked keys and provides a language to
enable productive discussions around contract sepcifications.

BSL is best described with some examples.

## Example 1: Alice receives coinbase output as P2PKH


    blockchain initialize
    keypair alice
    coinbase_tx = transaction (
        txid: 1
        input: nil
        output: p2pkh alice
    )
    blockchain block (
        height: 1
        transactions: [coinbase_tx]
    )

`blockchain initialize` creates a simulated blockchain.

`keypair alice` creates a new keypair that is assigned to `alice`.

`transaction` is used a construct a transaction. Here we create a
coinbase transaction that takes no input and has a p2pkh output.

`p2pkh alice` generated the bitcoin scriptPubKey such that alice's
public key can generate the required scriptSig to spend the output.

`blockchain block` is a construct that moves the blockchain forward to
the given height with the list of `transactions` listed.

## Example 2: Alice spends coinbase to Alice and Bob multisig

This example continues from example 1.

First Alice spends the coinbase transaction from example 1 to a
multisig output.

    keypair bob
    alice_to_alice_and_bob = transaction (
        input: coinbase_tx signed by alice
        outputs: [
            multisig 2 of 2 alice and bob
        ]
    )
    blockchain block (
        height: 2
        transactions: [alice_to_alice_and_bob]
    )
    

`multisig 2 of 2 alice and bob` is a construct that generates the
scriptPubKey that can be spent once signed by both alice and bob.



