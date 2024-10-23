# tiny_evm

A Rust written toy VM for Ethereum smart contracts

## Status

The following smart contract (written in Solidity) can run:
* store_1: initialize a storage value && do_store to increment it with constant value (6)
* store_2: initialize a storage value && do_store to increment it with function argument (provided in call data)

## Compile examples

* cd resources
* docker run -v ./:/sources ethereum/solc:stable -o /sources/output --abi --bin --optimize --overwrite /sources/store_1.sol

For podman user, run this before `docker run ...`:

* export DOCKER_HOST=unix://${XDG_RUNTIME_DIR}/podman/podman.sock
