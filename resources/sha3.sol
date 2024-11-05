// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

contract HashContract {
    bytes32 private messageHash;

    event MyLog1(string message);
    
    function hash(string memory _message) public {
        emit MyLog1(_message);
        messageHash = keccak256(bytes(_message));
    }

    function getMessageHash() public view returns (bytes32) {
        return messageHash;
    }
}