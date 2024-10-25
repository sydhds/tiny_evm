extern crate core;

use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Formatter, LowerHex};
use std::path::Path;

use alloy_primitives::U256;
use alloy_primitives::I256;
use sha3::{Digest, Keccak256};
use num_enum::{FromPrimitive, IntoPrimitive};
use thiserror::Error;
use tracing::{info, debug, instrument};
use tracing_subscriber::EnvFilter;

fn main() -> Result<(), Box<dyn Error>> {

    tracing_subscriber::fmt()
        // .with_max_level(tracing::Level::DEBUG)
        .with_env_filter(EnvFilter::from_default_env()) // RUST_LOG env variable
        .init();


    let call_data_hash: Vec<u8> = {
        let to_hash = "do_store()";
        let mut hasher = Keccak256::new();
        hasher.update(to_hash);
        let result = hasher.finalize();
        info!("keccak256({}): {:0x?}", to_hash, result.as_slice());
        result.to_vec()
    };
    let mut call_data = [0u8; 36];
    call_data[0] = call_data_hash[0];
    call_data[1] = call_data_hash[1];
    call_data[2] = call_data_hash[2];
    call_data[3] = call_data_hash[3];

    exec(Path::new("resources/output/Store1.bin"), call_data.as_slice(), None)?;

    Ok(())
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, FromPrimitive, IntoPrimitive, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Opcode {

    // End of execution opcode
    STOP = 0x00,

    // Operation opcodes
    ADD = 0x01,
    MUL = 0x02,
    SUB = 0x03,
    DIV = 0x04,
    SDIV = 0x05,
    MOD = 0x06,
    SMOD = 0x07,
    ADDMOD = 0x08,
    MULMOD = 0x09,
    EXP = 0x0A,
    SIGNEXTEND = 0x0B,

    // Cmp opcodes
    LT = 0x10,
    GT = 0x11,
    SLT = 0x12,
    SGT = 0x13,
    EQ = 0x14,

    // bit manipulation opcodes
    ISZERO = 0x15,
    AND = 0x16,
    OR = 0x17,
    XOR = 0x18,
    NOT = 0x19,
    BYTE = 0x1A,
    SHL = 0x1B,
    SHR = 0x1C,
    SAR = 0x1D,

    // Hash opcode(s)
    SHA3 = 0x20,

    // Env opcodes
    ADDRESS = 0x30,
    BALANCE = 0x31,
    ORIGIN = 0x32,
    CALLER = 0x33,
    CALLVALUE = 0x34,
    CALLDATALOAD = 0x35,
    CALLDATASIZE = 0x36,
    CALLDATACOPY = 0x37,
    CODESIZE = 0x38,
    CODECOPY = 0x39,
    GASPRICE = 0x3A,
    EXTCODESIZE = 0x3B,
    EXTCODECOPY = 0x3C,
    RETURNDATASIZE = 0x3D,
    RETURNDATACOPY = 0x3E,
    EXTCODEHASH = 0x3F,

    // Block info opcodes
    BLOCKHASH = 0x40,
    COINBASE = 0x41,
    TIMESTAMP = 0x42,
    NUMBER = 0x43,
    DIFFICULTY = 0x44,
    GASLIMIT = 0x45,
    CHAINID = 0x46,
    SELFBALANCE = 0x47,
    BASEFEE = 0x48,
    BLOBHASH = 0x49,
    BLOBBASEFEE = 0x4A,

    // Stack opcodes
    POP = 0x50,

    // Memory opcodes
    MLOAD = 0x51,
    MSTORE = 0x52,
    MSTORE8 = 0x53,

    // Storage opcodes
    SLOAD = 0x54,
    SSTORE = 0x55,

    // Jump & pc opcodes
    JUMP = 0x56,
    JUMPI = 0x57,
    PC = 0x58,
    MSIZE = 0x59,
    GAS = 0x5A,
    JUMPDEST = 0x5B,

    // Transient storage opcodes
    TLOAD = 0x5C,
    TSTORE = 0x5D,

    // Memory opcode(s)
    MCOPY = 0x5E,

    // Push opcodes
    PUSH0 = 0x5F,
    PUSH1 = 0x60,
    PUSH2 = 0x61,
    PUSH3 = 0x62,
    PUSH4 = 0x63,
    PUSH5 = 0x64,
    PUSH6 = 0x65,
    PUSH7 = 0x66,
    PUSH8 = 0x67,
    PUSH9 = 0x68,
    PUSH10 = 0x69,
    PUSH11 = 0x6A,
    PUSH12 = 0x6B,
    PUSH13 = 0x6C,
    PUSH14 = 0x6D,
    PUSH15 = 0x6E,
    PUSH16 = 0x6F,
    PUSH17 = 0x70,
    PUSH18 = 0x71,
    PUSH19 = 0x72,
    PUSH20 = 0x73,
    PUSH21 = 0x74,
    PUSH22 = 0x75,
    PUSH23 = 0x76,
    PUSH24 = 0x77,
    PUSH25 = 0x78,
    PUSH26 = 0x79,
    PUSH27 = 0x7A,
    PUSH28 = 0x7B,
    PUSH29 = 0x7C,
    PUSH30 = 0x7D,
    PUSH31 = 0x7E,
    PUSH32 = 0x7F,

    // Dup opcodes
    DUP1 = 0x80,
    DUP2 = 0x81,
    DUP3 = 0x82,
    DUP4 = 0x83,
    DUP5 = 0x84,
    DUP6 = 0x85,
    DUP7 = 0x86,
    DUP8 = 0x87,
    DUP9 = 0x88,
    DUP10 = 0x89,
    DUP11 = 0x8A,
    DUP12 = 0x8B,
    DUP13 = 0x8C,
    DUP14 = 0x8D,
    DUP15 = 0x8E,
    DUP16 = 0x8F,

    // Swap opcodes
    SWAP1 = 0x90,
    SWAP2 = 0x91,
    SWAP3 = 0x92,
    SWAP4 = 0x93,
    SWAP5 = 0x94,
    SWAP6 = 0x95,
    SWAP7 = 0x96,
    SWAP8 = 0x97,
    SWAP9 = 0x98,
    SWAP10 = 0x99,
    SWAP11 = 0x9A,
    SWAP12 = 0x9B,
    SWAP13 = 0x9C,
    SWAP14 = 0x9D,
    SWAP15 = 0x9E,
    SWAP16 = 0x9F,

    // Log opcodes
    LOG0 = 0xA0,
    LOG1 = 0xA1,
    LOG2 = 0xA2,
    LOG3 = 0xA3,
    LOG4 = 0xA4,

    // Misc opcodes
    CREATE = 0xF0,
    CALL = 0xF1,
    CALLCODE = 0xF2,
    RETURN = 0xF3,
    DELEGATECALL = 0xF4,
    CREATE2 = 0xF5,
    STATICCALL = 0xFA,
    #[num_enum(default)]
    REVERT = 0xFD,
    SELFDESTRUCT = 0xFF,
}

impl Copy for Opcode {}
impl LowerHex for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0x}", u8::from(*self))
    }
}

#[derive(Debug)]
struct Context {
    call_data: Vec<u8>,
    msg_value: U256,
    /// Ethereum chain id
    /// chain_id = {  1 // mainnet
    //        {  2 // Morden testnet (disused)
    //        {  2 // Expanse mainnet
    //        {  3 // Ropsten testnet
    //        {  4 // Rinkeby testnet
    //        {  5 // Goerli testnet
    //        { 42 // Kovan testnet
    //        { ...
    chain_id: U256,
}

#[derive(Debug)]
struct Db {
    stack: Vec<U256>,
    memory: Vec<u8>,
    storage: HashMap<U256, U256>,
    transient_storage: HashMap<U256, U256>,
    contract_offset: Option<usize>,
}

#[derive(Debug)]
enum ExecEndReason {
    Stop,
    Return(usize), // pc value
    Revert,
}

#[derive(Error, Debug)]
enum TinyEvmError {
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error(transparent)]
    HexError(#[from] hex::FromHexError),
}

#[instrument(skip_all, level="debug")]
fn exec(sc_path: &Path, call_data: &[u8], msg_value: Option<U256>) -> Result<(Db, Context), TinyEvmError> {

    let store_num_ = std::fs::read_to_string(sc_path)?;
    let bytecode = hex::decode(store_num_)?;

    let mut db = Db {
        stack: Default::default(),
        memory: vec![0; 4096],
        storage: Default::default(),
        transient_storage: Default::default(),
        contract_offset: None,
    };
    let mut ctx = Context {
        call_data: call_data.to_vec(),
        msg_value: U256::from(0),
        chain_id: U256::from(5),
    };

    let res_init = exec_bytecode(bytecode.as_slice(), &mut db, &mut ctx)?;
    if let ExecEndReason::Return(_pc) = res_init {
        debug!("contract_offset: {:0x?}", db.contract_offset);
        debug!("{}", "#".repeat(32));
        let sc_offset = db.contract_offset.clone();
        debug!("sc_offset: {:?}", sc_offset);
        // SC contract part
        // ctx.msg_value = U256::from(1000000000000000u64); // == 1M Gwei == 0.001 Ethereum
        // ctx.msg_value = U256::from(0u64);
        ctx.msg_value = msg_value.unwrap_or(U256::ZERO);
        let res_sc = exec_bytecode(bytecode.as_slice(), &mut db, &mut ctx)?;

        if let ExecEndReason::Stop = res_sc {
            info!("Successfully executed the bytecode for: {:?} !", sc_path);
        } else {
            panic!("Error while executing sc part of the bytecode, expecting Stop but got {:?}", res_sc);
        }
    } else {
        panic!("Error while executing init part of bytecode, expecting Return but got {:?}", res_init);
    }

    Ok((db, ctx))
}

/// Execute EVM bytecode
#[instrument(ret, skip_all, level="debug")]
fn exec_bytecode(bytecode: &[u8], db: &mut Db, ctx: &mut Context) -> Result<ExecEndReason, TinyEvmError> {

    #[allow(non_snake_case)]
    let U256_ONE: U256 = U256::from(1);
    let mut pc = db.contract_offset.unwrap_or(0);
    let mut res = Ok(ExecEndReason::Stop);
    debug!("pc: {}", pc);

    loop {
        // let opcode = bytecode[pc];
        let opcode = Opcode::from(bytecode[pc]);
        pc += 1;

        match opcode {
            Opcode::STOP => {
                debug!("OPCODE: STOP");
                break;
            },
            Opcode::ADD => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                db.stack.push(a + b);
                debug!("OPCODE: SUB");
            },
            Opcode::SUB => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                db.stack.push(a - b);
                debug!("OPCODE: SUB");
            },
            Opcode::LT => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                let to_push = if a < b {
                    U256_ONE
                } else {
                    U256::ZERO
                };
                db.stack.push(to_push);
                debug!("OPCODE: LT");
            },
            Opcode::GT => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                let to_push = if a > b {
                    U256_ONE
                } else {
                    U256::ZERO
                };
                db.stack.push(to_push);
                debug!("OPCODE: GT");
            },
            Opcode::SLT => {
                // FIXME: int256 comparison - but we have U256 in stack
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                let a = I256::try_from(a).unwrap();
                let b = I256::try_from(b).unwrap();
                let to_push = if a < b {
                    U256_ONE
                } else {
                    U256::ZERO
                };
                db.stack.push(to_push);
                debug!("OPCODE: SLT");
            },
            Opcode::EQ => {
                let from_stack_0 = db.stack.pop().unwrap();
                let from_stack_1 = db.stack.pop().unwrap();
                let to_push = if from_stack_0 == from_stack_1 { U256_ONE } else { U256::ZERO };
                db.stack.push(to_push);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: EQ");
            },
            Opcode::ISZERO => {
                let from_stack_0 = db.stack.pop().unwrap();
                let to_push = if from_stack_0 == U256::ZERO { U256_ONE } else { U256::ZERO };
                db.stack.push(to_push);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: ISZERO");
            }
            Opcode::SHR => {
                let shift_by = db.stack.pop().unwrap();
                let shift_by: usize = shift_by.try_into().unwrap();
                let to_shift = db.stack.pop().unwrap();
                let res = to_shift.rotate_right(shift_by);
                db.stack.push(res);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: SHR: {:0x?}", res.to_be_bytes::<32>()); // bit shift right
            },
            Opcode::CALLVALUE => {
                db.stack.push(ctx.msg_value);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE CALLVALUE");
            },
            Opcode::CALLDATALOAD => {
                let offset = db.stack.pop().unwrap();
                let offset: usize = offset.try_into().unwrap();
                let to_push = &ctx.call_data[offset..offset+32];
                let to_push = U256::from_be_bytes::<32>(to_push.try_into().unwrap());
                db.stack.push(to_push);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: CALLDATALOAD");
            },
            Opcode::CALLDATASIZE => {
                let to_push = U256::from(ctx.call_data.len());
                db.stack.push(to_push);
                debug!("OPCODE: CALLDATASIZE");
            },
            Opcode::CODECOPY => {
                let dest_offset = db.stack.pop().unwrap();
                let offset = db.stack.pop().unwrap();
                let length = db.stack.pop().unwrap();
                // Note: no need to do the actual copy as we are going to execute from bytecode
                db.contract_offset = Some(offset.try_into().unwrap());
                debug!("OPCODE: CODECOPY - dest_offset: {} - offset: {} - len: {}", dest_offset, offset, length);
            },
            Opcode::CHAINID => {
                db.stack.push(ctx.chain_id);
                debug!("OPCODE: CHAINID");
            },
            Opcode::POP => {
                db.stack.pop().unwrap();
                debug!("OPCODE: POP");
            }
            Opcode::MSTORE => {
                let offset = db.stack.pop().unwrap();
                let offset: usize = offset.try_into().unwrap();
                let value = db.stack.pop().unwrap();
                let memory_slice = &mut db.memory.as_mut_slice()[offset..offset+32];
                memory_slice.copy_from_slice(&value.to_be_bytes::<32>());
                debug!("OPCODE: MSTORE, offset: {}, value: {}", offset, value);
            },
            Opcode::SLOAD => {
                let key = db.stack.pop().unwrap();
                let value = db.storage.get(&key).unwrap();
                db.stack.push(*value);
                debug!("OPCODE: SLOAD");
            }
            Opcode::SSTORE => {
                let key = db.stack.pop().unwrap();
                let value = db.stack.pop().unwrap();
                db.storage.insert(key, value);
                debug!("Storage: {:?}", db.storage);
                debug!("OPCODE: SSTORE");
            },
            Opcode::JUMP => {
                let pc_0 = pc;
                let destination = db.stack.pop().unwrap();
                let destination: usize = destination.try_into().unwrap();
                if let Some(jump_offset) = db.contract_offset {
                    pc = destination + jump_offset;
                } else {
                    pc = destination;
                }
                // At the destination, we expect to have a JUMPDEST opcode (0x5B)
                // println!("OPCODE: JUMP, pc: {} (pc was: {})", pc, pc_0);
                let opcode_at_dest = bytecode[pc];
                if opcode_at_dest != 0x5B {
                    // TODO: Err
                    debug!("JUMP destination is not a JUMPDEST opcode");
                    break;
                } else {
                    pc += 1;
                }
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: JUMP, pc: {} (pc was: {})", pc, pc_0);
            }
            Opcode::JUMPI => {
                let pc_0 = pc;
                let destination = db.stack.pop().unwrap();
                let condition = db.stack.pop().unwrap();
                debug!("destination: 0x{:0x}", destination);
                debug!("condition: 0x{:0x}", condition);
                if condition == U256_ONE {
                    let destination: usize = destination.try_into().unwrap();
                    if let Some(jump_offset) = db.contract_offset {
                        pc = destination + jump_offset;
                    } else {
                        pc = destination;
                    }
                    // At the destination, we expect to have a JUMPDEST opcode (0x5B)
                    let opcode_at_dest = bytecode[pc];
                    debug!("opcode_at pc {}: {:0x?}", pc, opcode_at_dest);
                    if opcode_at_dest != 0x5B {
                        // FIXME: Err
                        debug!("JUMPI destination is not a JUMPDEST opcode but a {:0x} opcode", opcode_at_dest);
                        // break;
                    } else {
                        pc += 1;
                    }
                }
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: JUMPI, pc: {} (pc was: {})", pc, pc_0);
            }, /*
            0x5B => {
                println!("OPCODE: JUMPDEST");
            }, */
            Opcode::TLOAD => {
                let key = db.stack.pop().unwrap();
                let value = db.transient_storage.get(&key).unwrap();
                db.stack.push(*value);
                debug!("OPCODE: TLOAD");
            }
            Opcode::TSTORE => {
                let key = db.stack.pop().unwrap();
                let value = db.stack.pop().unwrap();
                db.transient_storage.insert(key, value);
                debug!("OPCODE: TSTORE");
            },
            Opcode::PUSH0 => {
                db.stack.push(U256::ZERO);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: PUSH_0");
            },
            opcode_ if (Opcode::PUSH1..=Opcode::PUSH32).contains(&opcode_) => {
                // PUSH_1 ... PUSH_32
                let push_idx = u8::from(opcode) - 0x60 + 1;
                let push_idx_ = usize::from(push_idx);
                let values = &bytecode[pc..(pc+push_idx_)];
                pc += push_idx_;
                let to_push = U256::from_be_slice(values);
                db.stack.push(to_push);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: PUSH{}, hex value: {:0x?} - value: {}", push_idx.to_ascii_uppercase(), values, to_push);
            },
            opcode_ if (Opcode::DUP1..=Opcode::DUP16).contains(&opcode_) => {
                let push_idx = u8::from(opcode) - 0x80 + 1;
                let push_idx_ = usize::from(push_idx);
                let stack_at = db.stack[db.stack.len() - push_idx_];
                db.stack.push(stack_at);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: DUP{}", push_idx.to_ascii_uppercase());
            },
            opcode_ if (Opcode::SWAP1..=Opcode::SWAP16).contains(&opcode_) => {
                let push_idx = u8::from(opcode) - 0x90 + 1;
                let push_idx_ = usize::from(push_idx);
                let index_last = db.stack.len() - 1;
                let index_to_swap = db.stack.len() - push_idx_ - 1;
                let tmp = db.stack[index_last];
                db.stack[index_last] = db.stack[index_to_swap];
                db.stack[index_to_swap] = tmp;
                debug!("OPCODE: SWAP{}", push_idx.to_ascii_uppercase());
            },
            Opcode::RETURN => {
                let offset = db.stack.pop().unwrap();
                let length = db.stack.pop().unwrap();
                debug!("OPCODE: RETURN - offset: {} - length: {}", offset, length);
                debug!("[END RETURN] pc: 0x{:0x} - pc: {}", pc, pc);
                res = Ok(ExecEndReason::Return(pc));
                break;
            },
            Opcode::REVERT => {
                debug!("OPCODE: REVERT");
                res = Ok(ExecEndReason::Revert);
                break;
            },
            _ => unimplemented!("opcode {:x} not implemented", opcode),
        }

        if pc > bytecode.len() - 1 {
            debug!("[END BYTECODE] pc: {:0x}", pc);
            break;
        }
    }

    res
}


#[cfg(test)]
mod test {
    use super::*;
    use tracing_test::traced_test;

    #[test]
    #[traced_test]
    fn test_store_1() -> Result<(), TinyEvmError> {

        let call_data_hash: Vec<u8> = {
            let to_hash = "do_store()";
            let mut hasher = Keccak256::new();
            hasher.update(to_hash);
            let result = hasher.finalize();
            result.to_vec()
        };

        let mut call_data = [0u8; 36];
        call_data[0] = call_data_hash[0];
        call_data[1] = call_data_hash[1];
        call_data[2] = call_data_hash[2];
        call_data[3] = call_data_hash[3];
        debug!("call_data (len: {}): {:0x?}", call_data.len(), call_data);

        let (db, _) = exec(Path::new("resources/output/Store1.bin"), call_data.as_slice(), None)?;

        let storage = db.storage;
        let value_0 = storage.get(&U256::ZERO).unwrap();
        assert_eq!(*value_0, U256::from(42));

        Ok(())
    }

    #[test]
    #[traced_test]
    fn test_store_2() -> Result<(), TinyEvmError> {

        let call_data_hash: Vec<u8> = {
            let to_hash = "do_store(uint256)";
            let mut hasher = Keccak256::new();
            hasher.update(to_hash);
            let result = hasher.finalize();
            result.to_vec()
        };

        let call_data_arg = U256::from(12).to_be_bytes_vec();
        let call_data: Vec<u8> = call_data_hash
            .into_iter()
            .take(4)
            .chain(call_data_arg)
            .collect();

        debug!("call_data (len: {}): {:0x?}", call_data.len(), call_data);

        let (db, _) = exec(Path::new("resources/output/Store2.bin"), call_data.as_slice(), None)?;

        let storage = db.storage;
        let value_0 = storage.get(&U256::ZERO).unwrap();
        assert_eq!(*value_0, U256::from(42));

        Ok(())
    }

    #[test]
    #[traced_test]
    fn test_store_3() -> Result<(), TinyEvmError> {

        let call_data_hash: Vec<u8> = {
            let to_hash = "do_store()";
            let mut hasher = Keccak256::new();
            hasher.update(to_hash);
            let result = hasher.finalize();
            result.to_vec()
        };

        let mut call_data = [0u8; 36];
        call_data[0] = call_data_hash[0];
        call_data[1] = call_data_hash[1];
        call_data[2] = call_data_hash[2];
        call_data[3] = call_data_hash[3];
        debug!("call_data (len: {}): {:0x?}", call_data.len(), call_data);

        let (db, ctx) = exec(Path::new("resources/output/Store3.bin"), call_data.as_slice(), None)?;

        let storage = db.storage;
        let value_0 = storage.get(&U256::ZERO).unwrap();
        assert_eq!(*value_0, U256::from(36)+ctx.chain_id);

        Ok(())
    }
}