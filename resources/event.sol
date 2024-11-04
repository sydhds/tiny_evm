// SPDX-License-Identifier: MIT
pragma solidity ^0.8.26;

contract AddAndEvent {
  
    event MyLog0(uint256 value);
    event MyLog1(address indexed sender, string message);
    event MyLog2(address indexed sender, string message, uint256 value);

    function and_and_event(uint256 x) public {
  	    emit MyLog0(1);
  	    emit MyLog0(x);
  	    uint256 z = x + 128;
  	    emit MyLog0(z);
        emit MyLog1(msg.sender, "Hello World from MyLog1 !");
  	    emit MyLog2(msg.sender, "Hello World from MyLog2 !", z);
    }
}
