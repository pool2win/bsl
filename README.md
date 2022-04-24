
## Introduction

Bitcoin Scripting Language (BSL) is useful for writing bitcoin
contracts that can be tested with a simulated blockchain. BSL can be
used for writing contracts that require out of band communication
between participants - enabling writing and testing of contracts that
require exchanging partially signed transactions or preimages of
secrets or using tweaked keys. Both bitcoin contracts and the
blockchain are described in BSL.

BSL is best described with examples. The example below shows a
coinbase transaction and how it is spent into a HTLC that is also
later spent.

## example: Spending to and from an HTLC

	```
	# Define keys
	key alice, bob, carol
	
	# Create coinbase with p2pkh contract and confirm it
	alice_coins = p2pkh alice amount 50 confirmation height 100
	
	# Define an HTLC contract with "secret" as the preimage
	htlc_contract = htlc from alice to bob timelock 1000 preimage "secret" amount 10

	# Spend alice coins to an htlc
	bob_coins = spend alice_coins signedby alice receiver htlc_contract confirmation height 200
	
	# Spend bob's htlc contract to carol by revealing preimage
	carol_coins = p2pkh carol amount 10
	spend htlc_contract signedby bob revealing "secret" receiver carol_coins confirmation height 300
	```

## Example: One Way Payment Channel

	```
	# Define keys
	key alice, bob
	
	# Create coinbase with p2pkh contract and confirm it
	bob_coins = p2pkh bob amount 50 confirmation height 100

	# Multisig contract, that is the channel
	channel = p2sh multisig 2 of 2 alice bob amount 50

	# Bond transaction
	bond = spend bob_coins signedby bob receiver channel unconfirmed

	# bob p2pkh for refund
	bob_return = p2pkh bob amount 50

	# Alice signs bond transaction after verifying bond spends to channel
	refund = spend channel signedby alice receiver bob_return after 1000 unconfirmed

	# Bob sends bond to alice, who signs and broadcasts it
	spend bond signedby alice receiver channel confirmation height 200 

	# Make micropayments over the channel
	channel_v1 = p2sh multisig 2 of 2 alice bob amounts alice 10 bob 40
	pay_alice_v1 = spend channel signedby bob receiver channel_v1 unconfirmed

	channel_v2 = p2sh multisig 2 of 2 alice bob amounts alice 20 bob 30
	pay_alice_v2 = spend channel signedby bob receiver channel_v2 unconfirmed
	
	# alice closes by spending v2
	spend pay_alice_v2 signedby alice receiver channel_v2 confirmation height 300	
	```

There is a fair bit happening in the above example scripts, we use
high level constructs like `p2pkh`, `htlc`, `sign`, `spend` and
`confirmation height` to write out a series of statements that help us
describe the workings of a contract that BSL execution engine can then
run. Such scripts can be used to try out bitcoin contracts and even
more importantly can be used to communicate ideas for bitcon contracts
within the community.

We can also use assertions to check the validity of our scripts. With
the script in the above example, we can assert if a UTXO has been
spent and verify they were spent to a specific output.

	```
	assert alice_coins are spent to htlc_contract
	```

Note, unlike miniscript, the goal of BSL is not to generate production
ready, optimised bitcoin Scripts. For generating bitcoin Scripts in
production contracts, miniscript is the best choice at the moment.

We next show how BSL can be used to compose contracts.


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



# Related Work

https://bitcoin.sipa.be/miniscript/
https://min.sc/

https://docs.ivylang.org/bitcoin/
https://github.com/spruceid/cryptoscript
https://scrypt.studio/
https://github.com/sapio-lang/sapio/tree/master/sapio

https://chialisp.com/
https://docs.stacks.co/write-smart-contracts/overview
