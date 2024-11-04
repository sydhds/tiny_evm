# tiny_evm

A toy VM for Ethereum smart contracts, written in Rust from scratch with minimal dependencies.

## Dependencies

* alloy-primitives: I256 & U256 types
* sha3: keccak256 implementation
* hex: decode hex strings

## Status

The following smart contract (written in Solidity) can run successfully:
* store_1.sol: initialize a storage value && do_store to increment it with constant value (6)
* store_2.sol: initialize a storage value && do_store to increment it with function argument (provided in call data)
* event.sol: log events (with 3 event types)


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