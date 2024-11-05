# tiny_evm

A toy VM for Ethereum smart contracts, written in Rust from scratch with minimal dependencies.

## Dependencies

* alloy-primitives: I256 & U256 types
* sha3: keccak256 implementation
* hex: decode hex strings

## Status

* Implemented:
  * Opcodes: 110 / 148 (~ 75%)
  * Gas: N/A
  * Memory / Storage / Transient storage / Event log: Yes

The following smart contract (written in Solidity) can run successfully:
* store_1.sol: initialize a storage value && do_store to increment it with constant value (6)
* store_2.sol: initialize a storage value && do_store to increment it with function argument (provided in call data)
* event.sol: log events (with 3 event types)
* sha3.sol: keccak256 opcode + call data arguments encode

## Run a Smart contract

### Setup

* cargo build

### Quickstart

* ./target/debug/tiny_evm --sc resources/output/Store1.bin

Note: call_data for Store1.bin is automatically computed (for this quickstart).

### Howto

Provide Smart contract bin file path && call data (as a hex string):

* RUST_LOG=debug ./target/debug/tiny_evm --sc resources/output/AddAndEvent.bin -c 5b3ee9310000000000000000000000000000000000000000000000000000000000000000

## Compile examples

* cd resources
* docker run -v ./:/sources ethereum/solc:stable -o /sources/output --abi --bin --optimize --overwrite /sources/store_1.sol

For podman user, run this before `docker run ...`:

* export DOCKER_HOST=unix://${XDG_RUNTIME_DIR}/podman/podman.sock

# Resources

* https://noxx.substack.com/p/evm-deep-dives-the-path-to-shadowy
* opcodes:
  * https://www.ethervm.io/
* https://www.youtube.com/watch?v=RxL_1AfV7N4&t=2s

# TODO

* Add example with contract call
* Add faster stack impl
  * idea: https://www.reddit.com/r/rust/comments/1g96xpd/made_a_stack_based_vm/
* More opcodes && more optimizations
