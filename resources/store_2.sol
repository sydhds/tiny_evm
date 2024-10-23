// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

contract Store2 {
    uint256 public num = 30;

    function do_store(uint256 i) public {
    	num += i;
    }
}
