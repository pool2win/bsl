
## Introduction

Bitcoin Scripting Language (BSL) is useful for writing bitcoin
contracts that can be tested with a simulated blockchain. Both bitcoin
contracts and the blockchain are described in BSL.

BSL is a DSL that can be used to describe a bitcoin contract, encumber
UTXOs with bitcoin contracts and finally describe transactions that
can be used to spend the encumbered UTXO.

BSL enables writing contracts that use high level concepts like
bitcoin standard scripts and tweaked keys to provide a language that
enables testing contracts.

BSL is best described with an example. The example below shows a
coinbase transaction and how it is spent into a contract that is also
later spent.

	```
	# Define keys
	key alice, bob, carol
	
	# Create a coinbase transaction and a genesis block
	coinbase_tx = transaction(
		inputs: [], 
		outputs: [p2pkh(receiver: alice, amount: 50)]
	)
	genesis_block = block(transactions: [coinbase_tx])
	
	# Define an HTLC contract with hash h	
	h = hash(preimage: "secret")
	htlc_contract = htlc(
		local: alice,
		remote: bob,
		lock_time: 1000,
		hash: h,
		amount: 10
	)
	
	# Spend coinbase tx to htlc contract and snd change back to alice
	spend_coinbase = transaction(
		inputs: [coinbase_tx:0],
		outputs: [htlc_contract, 
			      p2pkh(receiver: alice, amount: 40)]
	)
	
	signed_spend_coinbase = sign(tx: spend_coinbase, key: alice)

	# Create a second block, spending spend_coinbase tx
	second_block = block(transactions: [signed_spend_coinbase], previous_block: genesis_block)

	# Create a transaction spending htlc contract
	spend_htlc = transaction(inputs: [spend_coinbase:0], outputs: [p2pkh(receiver: carol, amount 10)])
	
	# Unlock spend coinbase with remote key for bob and the hash preimage
	sender_spend_htlc = sign(tx: spend_htlc, key: bob, reveal: "secret")
	
	# generate 1000 blocks (move time forward)
	last_block = generate_blocks(count: 1000)
	
	# Create block, spending sender_spend_htlc after 1000 blocks
	third_block = block(transactions: [signed_spend_htlc], previous_block: last_block)
	```

There is a fair bit happening in the above script, we use high level
constructs like `p2pkh`, `htlc`, `block`, `transaction` and `sign` to
write out a series of statements that help us describe the workings of
a contract that BSL can execute. Such scripts can be used to try out
bitcoin contracts and even more importantly can be used to communicate
ideas for bitcon contracts within the community.

We can also use assertions to check the validity of our scripts. With
the script in the above example, we can assert if a UTXO has been
spent at a certain height.

	```
	# assert coinbase output is spent by second_block
	assert_not spent_at(output: coinbase_tx:0, height: 0)
	assert spent_at(output: coinbase_tx:0, height: 1)
	```

Note, unlike miniscript, the goal of BSL is not to generate production
ready, optimised bitcoin Scripts. For generating bitcoin Scripts in
production contracts, miniscript is the best choice at the moment.

Now that we have seen a script that generates and spends transactions
across blocks, we now show how BSL can be used to compose
contracts. We start with some simple examples and later get into
contracts that require composition of contracts.


## Example: Define a key

`key alice`

## Example: Describing Contracts

1. A single key

	```
	key alice
	p2pkh alice
	```

2. One of two keys (equally likely)

	```
	key alice, bob
	p2pkh alice or p2pkh bob
	```

3. Alice and a 2FA service need to sign off, but after 90 days the user alone is enough

	```
	key alice, key_service
	p2pkh alice and (p2pkh key_service and timelock for 12960)
	```

4. A 3-of-3 that turns into a 2-of-3 after 90 days (~12960 blocks)



5. The BOLT #3 to_local policy
6. The BOLT #3 offered HTLC policy
7. The BOLT #3 received HTLC policy

## Example: Alice receives coinbase output as P2PKH


```
    blockchain initialize
    keypair alice
    coinbase_tx = transaction (
        txid: 1
        inputs: []
        outputs: [p2pkh alice]
    )
    blockchain block (
        height: 1
        transactions: [coinbase_tx]
    )
```

`blockchain initialize` creates a simulated blockchain.

`keypair alice` creates a new keypair that is assigned to `alice`.

`transaction` is used a construct a transaction. Here we create a
coinbase transaction that takes no input and has a p2pkh output.

`p2pkh alice` generated the bitcoin scriptPubKey such that alice's
public key can generate the required scriptSig to spend the output.

`blockchain block` is a construct that moves the blockchain forward to
the given height with the list of `transactions` listed.

## Example: Alice spends coinbase to Alice and Bob multisig

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


# Development

To run tests `raco test .` for now. No package support is provided yet.

