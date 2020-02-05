
See the [Tezos Client Tutotial](https://assets.tqtezos.com/setup/1-tezos-client) and the
FA1.2 [Quick Start Tutorial](https://assets.tqtezos.com/token-contracts/1-fa12-lorentz) for more detail.

# Test contract

For testing, we have a simple contract with two entrypoints:

```
❯❯❯ alpha-client get contract entrypoints for $TRUE_OR_42_ADDRESS

Entrypoints for contract KT1F67Mdv7V3ynJSRBefVbVV7iijaCUu3Gog: 
  default: (or (bool %send_true) (nat %send_42))
  send_true: bool
  send_42: nat
```

- `send_true` fails unless `True` is sent
- `send_42` fails unless `42` is sent

It can be originated as follows:

```bash
❯❯❯ alpha-client --wait none originate contract TrueOr42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(cat true_or_42.tz | tr '\n' ' ')" \
  --burn-cap 0.406

Waiting for the node to be bootstrapped before injection...
Current head: BKvDbkmi1x1x (timestamp: 2020-02-04T20:15:38-00:00, validation: 2020-02-04T20:16:10-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 13298 units (will add 100 for safety)
Estimated storage: 406 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'op3scSdMHzHkKQJFjwKrnSLDrLd52qhbeddtqVLf34pSMRCRAwT'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for op3scSdMHzHkKQJFjwKrnSLDrLd52qhbeddtqVLf34pSMRCRAwT to be included --confirmations 30 --branch BKvDbkmi1x1xc13kZXui78XpC5jw4HxURkKg2cUvsPmsqFyyVo3
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.001718
    Expected counter: 33125
    Gas limit: 13398
    Storage limit: 426 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.001718
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,128) ... +ꜩ0.001718
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (bool %send_true) (nat %send_42)) ;
          storage unit ;
          code { CAR ;
                 IF_LEFT { ASSERT } { PUSH nat 42 ; ASSERT_CMPEQ } ;
                 UNIT ;
                 NIL operation ;
                 PAIR } }
        Initial storage: Unit
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1F67Mdv7V3ynJSRBefVbVV7iijaCUu3Gog
        Storage size: 149 bytes
        Paid storage size diff: 149 bytes
        Consumed gas: 13298
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.149
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1F67Mdv7V3ynJSRBefVbVV7iijaCUu3Gog originated.
Contract memorized as TrueOr42.
```

Set an alias for it:

```bash
❯❯❯ TRUE_OR_42_ADDRESS="KT1F67Mdv7V3ynJSRBefVbVV7iijaCUu3Gog" 
```


# Wrapping the test contract

To print the wrapped (authenticated) contract where:
- Entrypoints _do not_ require authentication by default
- The `send_42` entrypont _does_ require authentication

```bash
❯❯❯ ./stack exec -- lorentz-contract-authenticated Authenticated print \
  --default-authenticated False \
  --authenticated-entrypoints "[(\"send_42\",True)]" \
  --wrapped "$(cat true_or_42.tz)" --oneline

parameter (or (or bool nat) (or address (pair unit (contract address))));storage (pair unit address);code { DUP;CAR;DIP { CDR };IF_LEFT { DIP { DUP;CAR;DIP { CDR };SWAP };DUP;IF_LEFT { DROP;PUSH bool False }        { DROP;PUSH bool True };IF { DIP { DUP;DIP { SENDER;COMPARE;EQ;IF {  }   { PUSH string "only admin";FAILWITH } } } }   {  };SWAP;DIP { PAIR;CAR;IF_LEFT { { IF {  }   { UNIT;FAILWITH } } }        { PUSH nat 42;{ { COMPARE;EQ;IF {  }   { UNIT;FAILWITH } } } };UNIT;NIL operation;PAIR;DUP;CAR;DIP { CDR } };SWAP;DIP { SWAP;PAIR };PAIR }        { IF_LEFT { DIP { DUP;CAR;DIP { CDR };SWAP;SENDER;COMPARE;EQ;IF {  }   { PUSH string "only admin";FAILWITH } };SWAP;PAIR;NIL operation;PAIR }        { DUP;CAR;DIP { CDR };DIP { DIP { DUP };SWAP };PAIR;CDR;CDR;DIP { AMOUNT };TRANSFER_TOKENS;NIL operation;SWAP;CONS;PAIR } } };
```


Since the `true_or_42` contract has `unit` storage,
we provide the type and value along with an admin
address for `alice`:

```bash
❯❯❯ ./stack exec -- lorentz-contract-authenticated Authenticated init \
  --initial-wrappedType "unit" \
  --initial-wrapped "Unit" \
  --admin "$ALICE_ADDRESS"
Pair Unit "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr"
```

Then we can originate the contract:

```bash
❯❯❯ alpha-client --wait none originate contract AuthenticatedTrueOr42 \
  transferring 0 from $BOB_ADDRESS running \
  "$(./stack exec -- lorentz-contract-authenticated Authenticated print \
  --default-authenticated False \
  --authenticated-entrypoints "[(\"send_42\",True)]" \
  --wrapped "$(cat true_or_42.tz)" --oneline)" \
  --init "$(./stack exec -- lorentz-contract-authenticated Authenticated init \
  --initial-wrappedType "unit" \
  --initial-wrapped "Unit" \
  --admin "$ALICE_ADDRESS")" --burn-cap 0.741

Waiting for the node to be bootstrapped before injection...
Current head: BKzN6HKG18Sy (timestamp: 2020-02-04T20:30:38-00:00, validation: 2020-02-04T20:31:03-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 22674 units (will add 100 for safety)
Estimated storage: 741 bytes added (will add 20 for safety)
Operation successfully injected in the node.
Operation hash is 'ong2RPcXaeQSpqFyJhx3XdgxDCz2yaHKo2C3beKxbnEbokN8S3E'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ong2RPcXaeQSpqFyJhx3XdgxDCz2yaHKo2C3beKxbnEbokN8S3E to be included --confirmations 30 --branch BKzN6HKG18Sy7BGtToZrfUKBPfBAvBwDfUBVCNfn8ikxrMwhWkp
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.003006
    Expected counter: 33126
    Gas limit: 22774
    Storage limit: 761 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.003006
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,128) ... +ꜩ0.003006
    Origination:
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      Credit: ꜩ0
      Script:
        { parameter (or (or bool nat) (or address (pair unit (contract address)))) ;
        ..
                         PAIR } } } }
        Initial storage: (Pair Unit "tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr")
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb
        Storage size: 484 bytes
        Paid storage size diff: 484 bytes
        Consumed gas: 22674
        Balance updates:
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.484
          tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ... -ꜩ0.257

New contract KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb originated.
Contract memorized as AuthenticatedTrueOr42.
```

And set a `bash` alias for it:

```bash
❯❯❯ AUTHENTICATED_TRUE_OR_42_ADDRESS="KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb"
```


# Sending parameters to the wrapped contract

The parameter anyone can send:

```bash
❯❯❯ ./stack exec -- lorentz-contract-authenticated Authenticated WrappedParam \
  --wrappedParamType "or bool nat" \
  --wrappedParam "Left True"
Left (Left True)
```

Sent by `alice`:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $AUTHENTICATED_TRUE_OR_42_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-authenticated Authenticated WrappedParam \
  --wrappedParamType "or bool nat" \
  --wrappedParam "Left True")"

Waiting for the node to be bootstrapped before injection...
Current head: BLwA8wQwmYa2 (timestamp: 2020-02-04T20:36:54-00:00, validation: 2020-02-04T20:37:00-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 21365 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'ooAcD6oEVVdQP2WfSkAxqUPH5gxZrt1wByJmhzQhEAE1eHgAmfo'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for ooAcD6oEVVdQP2WfSkAxqUPH5gxZrt1wByJmhzQhEAE1eHgAmfo to be included --confirmations 30 --branch BLwA8wQwmYa25ZRJ3Vk4C515y8Ebd2PB46nsajAF3bQRXah8hhs
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.002409
    Expected counter: 127467
    Gas limit: 21465
    Storage limit: 0 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............. -ꜩ0.002409
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,128) ... +ꜩ0.002409
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb
      Parameter: (Left (Left True))
      This transaction was successfully applied
      Updated storage:
        (Pair Unit 0x00003b5d4596c032347b72fb51f688c45200d0cb50db)
      Storage size: 484 bytes
      Consumed gas: 21365
```

Sent by `bob`:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to $AUTHENTICATED_TRUE_OR_42_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-authenticated Authenticated WrappedParam \
  --wrappedParamType "or bool nat" \
  --wrappedParam "Left True")"

Waiting for the node to be bootstrapped before injection...
Current head: BMTP5LZCiU6G (timestamp: 2020-02-04T20:34:54-00:00, validation: 2020-02-04T20:35:15-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 21365 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opDHYeaaG8EH6AKcLK2fGejT5myvTnTizp3niaQkFcXLhvadQwh'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opDHYeaaG8EH6AKcLK2fGejT5myvTnTizp3niaQkFcXLhvadQwh to be included --confirmations 30 --branch BMTP5LZCiU6GQCsKZ7YsecU1fmBgFN92y7E8nj1sqUT54CcZmqN
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0.002409
    Expected counter: 33127
    Gas limit: 21465
    Storage limit: 0 bytes
    Balance updates:
      tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm ............. -ꜩ0.002409
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,128) ... +ꜩ0.002409
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb
      Parameter: (Left (Left True))
      This transaction was successfully applied
      Updated storage:
        (Pair Unit 0x00003b5d4596c032347b72fb51f688c45200d0cb50db)
      Storage size: 484 bytes
      Consumed gas: 21365
```

The authenticated parameter (only `alice` is allowed to submit it):

```bash
./stack exec -- lorentz-contract-authenticated Authenticated WrappedParam \
  --wrappedParamType "or bool nat" \
  --wrappedParam "Right 42"
Left (Right 42)
```

Sent by `alice`:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $ALICE_ADDRESS to $AUTHENTICATED_TRUE_OR_42_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-authenticated Authenticated WrappedParam \
  --wrappedParamType "or bool nat" \
  --wrappedParam "Right 42")"

Waiting for the node to be bootstrapped before injection...
Current head: BL6sctQw1JtQ (timestamp: 2020-02-04T20:37:24-00:00, validation: 2020-02-04T20:37:49-00:00)
Node is bootstrapped, ready for injecting operations.
Estimated gas: 21377 units (will add 100 for safety)
Estimated storage: no bytes added
Operation successfully injected in the node.
Operation hash is 'opVhsqfrKecVPz3AkAj3HQ21Q9Zm13wd3WaHiFQnF9ZiH4FMSa3'
NOT waiting for the operation to be included.
Use command
  tezos-client wait for opVhsqfrKecVPz3AkAj3HQ21Q9Zm13wd3WaHiFQnF9ZiH4FMSa3 to be included --confirmations 30 --branch BL6sctQw1JtQ9nW57uhfP2eSG1oqvpaFjmiQS5k6To5SZiqtd2y
and/or an external block explorer to make sure that it has been included.
This sequence of operations was run:
  Manager signed operations:
    From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
    Fee to the baker: ꜩ0.00241
    Expected counter: 127468
    Gas limit: 21477
    Storage limit: 0 bytes
    Balance updates:
      tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr ............. -ꜩ0.00241
      fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,128) ... +ꜩ0.00241
    Transaction:
      Amount: ꜩ0
      From: tz1R3vJ5TV8Y5pVj8dicBR23Zv8JArusDkYr
      To: KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb
      Parameter: (Left (Right 42))
      This transaction was successfully applied
      Updated storage:
        (Pair Unit 0x00003b5d4596c032347b72fb51f688c45200d0cb50db)
      Storage size: 484 bytes
      Consumed gas: 21377
```

Sent by `bob`:

```bash
❯❯❯ alpha-client --wait none transfer 0 from $BOB_ADDRESS to $AUTHENTICATED_TRUE_OR_42_ADDRESS \
  --arg "$(./stack exec -- lorentz-contract-authenticated Authenticated WrappedParam \
  --wrappedParamType "or bool nat" \
  --wrappedParam "Right 42")"

Waiting for the node to be bootstrapped before injection...
Current head: BMRtGPoU7inj (timestamp: 2020-02-04T20:37:54-00:00, validation: 2020-02-04T20:38:16-00:00)
Node is bootstrapped, ready for injecting operations.
This simulation failed:
  Manager signed operations:
    From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
    Fee to the baker: ꜩ0
    Expected counter: 33128
    Gas limit: 800000
    Storage limit: 60000 bytes
    Transaction:
      Amount: ꜩ0
      From: tz1bDCu64RmcpWahdn9bWrDMi6cu7mXZynHm
      To: KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb
      Parameter: (Left (Right 42))
      This operation FAILED.

Runtime error in contract KT1XQ5BDFz2xJH5Msup5R1JBwuSFrqC8Kdjb:
  01: { parameter (or (or bool nat) (or address (pair unit (contract address)))) ;
  02:   storage (pair unit address) ;
  03:   code { DUP ;
  04:          CAR ;
  05:          DIP { CDR } ;
  06:          IF_LEFT
  07:            { DIP { DUP ; CAR ; DIP { CDR } ; SWAP } ;
  08:              DUP ;
  09:              IF_LEFT { DROP ; PUSH bool False } { DROP ; PUSH bool True } ;
  10:              IF { DIP { DUP ;
  11:                         DIP { SENDER ; COMPARE ; EQ ; IF {} { PUSH string "only admin" ; FAILWITH } } } }
  12:                 {} ;
  13:              SWAP ;
  14:              DIP { PAIR ;
  15:                    CAR ;
  16:                    IF_LEFT
  17:                      { { IF {} { UNIT ; FAILWITH } } }
  18:                      { PUSH nat 42 ; { IFCMPEQ {} { UNIT ; FAILWITH } } } ;
  19:                    UNIT ;
  20:                    NIL operation ;
  21:                    PAIR ;
  22:                    DUP ;
  23:                    CAR ;
  24:                    DIP { CDR } } ;
  25:              SWAP ;
  26:              DIP { SWAP ; PAIR } ;
  27:              PAIR }
  28:            { IF_LEFT
  29:                { DIP { DUP ;
  30:                        CAR ;
  31:                        DIP { CDR } ;
  32:                        SWAP ;
  33:                        SENDER ;
  34:                        COMPARE ;
  35:                        EQ ;
  36:                        IF {} { PUSH string "only admin" ; FAILWITH } } ;
  37:                  SWAP ;
  38:                  PAIR ;
  39:                  NIL operation ;
  40:                  PAIR }
  41:                { DUP ;
  42:                  CAR ;
  43:                  DIP { CDR } ;
  44:                  DIP { DIP { DUP } ; SWAP } ;
  45:                  PAIR ;
  46:                  CDR ;
  47:                  CDR ;
  48:                  DIP { AMOUNT } ;
  49:                  TRANSFER_TOKENS ;
  50:                  NIL operation ;
  51:                  SWAP ;
  52:                  CONS ;
  53:                  PAIR } } } }
At line 11 characters 89 to 97,
script reached FAILWITH instruction
with "only admin"
Fatal error:
  transfer simulation failed
```

