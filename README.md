
# Introduction

Bitcoin Scripting Language (BSL) is useful for writing bitcoin
contracts that can be tested with a simulated blockchain. BSL can be
used for writing contracts that require out of band communication
between participants - enabling writing and testing of contracts that
require exchanging partially signed transactions or preimages of
secrets or using tweaked keys. Both bitcoin contracts and the
blockchain are described in BSL.

BSL is best described with examples.

## Example: Simple Script spendable by one of two keys

This example shows a coinbase transaction and how it is spent into a
contract that can spent by two parties.

```
# Define keys
key alice, bob, carol

# Create coinbase with p2pkh contract and confirm it
alice_coins = p2pkh alice amount 50 confirmation height 100

# Send alice coins to either bob or carol
spend alice_coins signedby alice receiver p2pkh bob 50 or p2pkh carol 50 confirmation height 200
```

## Example: Spending to and from an HTLC

This example shows a coinbase transaction and how it is spent into
an HTLC and how the HTLC is spent as well.


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

Next we show how BSL can be used to specify a contract for a one way
channel.

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
more importantly can be used to communicate ideas for bitcoin contracts
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

## Example: Composing Higher Level Constructs

In the HTLC example above we used an construct `htlc`. We implied that
BSL provides the implementation of BSL as part of the execution
engine. In reality, the `htlc` is implemented using the inbuilt
operations provided by BSL.

```
define htlc from $sender to $receiver timelock $time preimage $secret amount $amount ->
	(p2pkh $receiver and reveal sha256 $secret) or (p2pkh $sender and after $time) amount $amount
```

`define` is used to compose new contracts from core constructs provided by BSL.


# Core Constructs

[TODO - List and explain core constructs supported by BSL]


# Development

To run tests `raco test .` for now. No package support is provided yet.

# Related Work

[TODO - Briefly describe related work and explain how BSL's goals are different. ]


1. https://bitcoin.sipa.be/miniscript/
2. https://min.sc/
3. https://docs.ivylang.org/bitcoin/
4. https://github.com/spruceid/cryptoscript
5. https://scrypt.studio/
6. https://github.com/sapio-lang/sapio/tree/master/sapio
7. https://chialisp.com/
8. https://docs.stacks.co/write-smart-contracts/overview
