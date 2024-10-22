// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

contract Store1 {
    uint256 public num = 36;
    function do_store() public {
    	num += 6;
    }
}
