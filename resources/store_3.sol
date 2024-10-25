// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

contract Store3 {
    uint256 public num = 36;
    function do_store() public {
        uint256 id;
        assembly {
            id := chainid()
        }
    	num += id;
    }
}
