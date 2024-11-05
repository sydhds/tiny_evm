extern crate core;

use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Formatter, LowerHex};
use std::ops::{Shl, Shr};
use std::path::Path;

use alloy_primitives::{address, U256};
use alloy_primitives::I256;
use alloy_primitives::Address;
use sha3::{Digest, Keccak256};
use num_enum::{FromPrimitive, IntoPrimitive};
use thiserror::Error;
use tracing::{info, debug, instrument};
use tracing_subscriber::EnvFilter;

fn main() -> Result<(), Box<dyn Error>> {

    tracing_subscriber::fmt()
        // .with_max_level(tracing::Level::DEBUG)
        // .with_env_filter(EnvFilter::from_default_env()) // RUST_LOG env variable
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
    caller: Address,
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
struct LogEvent {
    data: Vec<u8>,
    topics: Vec<U256>,
}

#[derive(Debug)]
struct Db {
    stack: Vec<U256>,
    memory: Vec<u8>,
    storage: HashMap<U256, U256>,
    transient_storage: HashMap<U256, U256>,
    contract_offset: Option<usize>,
    logs: Vec<LogEvent>,
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
        logs: vec![],
    };

    let caller = address!("d8da6bf26964af9d7eed9e03e53415d37aa96045");
    info!("caller address: {}", caller);
    let mut ctx = Context {
        caller,
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
                // (u)int256 addition modulo 2**256
                db.stack.push(a.wrapping_add(b));
                debug!("OPCODE: SUB");
            },
            Opcode::MUL => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                db.stack.push(a.wrapping_mul(b));
                debug!("OPCODE: MUL");
            },
            Opcode::SUB => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                db.stack.push(a.wrapping_sub(b));
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
            Opcode::SGT => {
                // FIXME: int256 comparison - but we have U256 in stack
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                let a = I256::try_from(a).unwrap();
                let b = I256::try_from(b).unwrap();
                let to_push = if a > b { U256_ONE } else { U256::ZERO };
                db.stack.push(to_push);
                debug!("OPCODE: SGT");
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
            },
            Opcode::AND => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                db.stack.push(a & b);
            },
            Opcode::OR => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                db.stack.push(a | b);
            },
            Opcode::XOR => {
                let a = db.stack.pop().unwrap();
                let b = db.stack.pop().unwrap();
                db.stack.push(a ^ b);
            },
            Opcode::NOT => {
                let a = db.stack.pop().unwrap();
                db.stack.push(!a);
            },
            Opcode::SHL => {
                let shift_by = db.stack.pop().unwrap();
                let shift_by: usize = shift_by.try_into().unwrap();
                let to_shift = db.stack.pop().unwrap();
                let res = to_shift.shl(shift_by);
                db.stack.push(res);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: SHL: {:0x?}", res.to_be_bytes::<32>()); // bit shift left
            },
            Opcode::SHR => {
                let shift_by = db.stack.pop().unwrap();
                let shift_by: usize = shift_by.try_into().unwrap();
                let to_shift = db.stack.pop().unwrap();
                let res = to_shift.shr(shift_by);
                db.stack.push(res);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: SHR: {:0x?}", res.to_be_bytes::<32>()); // bit shift right
            },
            Opcode::SHA3 => {
                let offset = db.stack.pop().unwrap();
                let offset = usize::try_from(offset).unwrap();
                let length = db.stack.pop().unwrap();
                let length = usize::try_from(length).unwrap();
                
                let mem_slice = &db.memory.as_slice()[offset..offset+length];
                debug!("Hashing mem slice: {:0x?}", mem_slice);
                let mut hasher = Keccak256::new();
                hasher.update(mem_slice);
                let result = hasher.finalize();
                let to_push = U256::from_be_slice(result.as_slice());
                debug!("Hashing result: {}", to_push);
                db.stack.push(to_push);
                // debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE SHA3");
            },
            Opcode::CALLER => {
                let addr = U256::from_be_slice(ctx.caller.as_slice());
                println!("addr: {}", addr);
                db.stack.push(addr);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE CALLER");
            }
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
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: CALLDATASIZE");
            },
            Opcode::CALLDATACOPY => {
                let dest_offset = db.stack.pop().unwrap();
                let dest_offset = usize::try_from(dest_offset).unwrap();
                let offset = db.stack.pop().unwrap();
                let offset = usize::try_from(offset).unwrap();
                let length = db.stack.pop().unwrap();
                let length = usize::try_from(length).unwrap();
                let memory_slice = &mut db.memory.as_mut_slice()[dest_offset..dest_offset+length];
                memory_slice.copy_from_slice(&ctx.call_data[offset..offset+length]);
                debug!("OPCODE: CALLDATACOPY");
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
            },
            Opcode::MLOAD => {
                let offset = db.stack.pop().unwrap();
                let offset: usize = offset.try_into().unwrap();
                let value = U256::from_be_slice(
                    &db.memory.as_slice()[offset..offset+32]
                );
                db.stack.push(value);
                debug!("OPCODE: MLOAD, offset: {}, value: {}", offset, value);
            },
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
            Opcode::MCOPY => {
                let dest_offset = db.stack.pop().unwrap();
                let dest_offset = usize::try_from(dest_offset).unwrap();
                let offset = db.stack.pop().unwrap();
                let offset = usize::try_from(offset).unwrap();
                let length = db.stack.pop().unwrap();
                let length = usize::try_from(length).unwrap();
                
                let to_copy = db.memory[offset..offset+length].to_vec();
                
                let memory_slice = &mut db.memory.as_mut_slice()[dest_offset..dest_offset+length];
                memory_slice.copy_from_slice(to_copy.as_slice());
                debug!("OPCODE: MCOPY");
            },
            Opcode::PUSH0 => {
                db.stack.push(U256::ZERO);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: PUSH_0");
            },
            opcode_ if (Opcode::PUSH1..=Opcode::PUSH32).contains(&opcode_) => {
                // PUSH_1 ... PUSH_32
                let push_idx = u8::from(opcode) - u8::from(Opcode::PUSH1) + 1;
                let push_idx_ = usize::from(push_idx);
                let values = &bytecode[pc..(pc+push_idx_)];
                pc += push_idx_;
                let to_push = U256::from_be_slice(values);
                db.stack.push(to_push);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: PUSH{}, hex value: {:0x?} - value: {}", push_idx.to_ascii_uppercase(), values, to_push);
            },
            opcode_ if (Opcode::DUP1..=Opcode::DUP16).contains(&opcode_) => {
                let push_idx = u8::from(opcode) - u8::from(Opcode::DUP1) + 1;
                let push_idx_ = usize::from(push_idx);
                let stack_at = db.stack[db.stack.len() - push_idx_];
                db.stack.push(stack_at);
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: DUP{}", push_idx.to_ascii_uppercase());
            },
            opcode_ if (Opcode::SWAP1..=Opcode::SWAP16).contains(&opcode_) => {
                debug!("OPCODE: SWAPX - stack: {:0x?}", db.stack);
                let push_idx = u8::from(opcode) - u8::from(Opcode::SWAP1) + 1;
                let push_idx_ = usize::from(push_idx);
                let index_last = db.stack.len() - 1;
                let index_to_swap = db.stack.len() - push_idx_ - 1;
                let tmp = db.stack[index_last];
                db.stack[index_last] = db.stack[index_to_swap];
                db.stack[index_to_swap] = tmp;
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE: SWAP{}", push_idx.to_ascii_uppercase());
            },
            opcode_ if (Opcode::LOG0..=Opcode::LOG4).contains(&opcode_) => {
                debug!("OPCODE: LOGX - stack: {:0x?}", db.stack);
                let offset = db.stack.pop().unwrap();
                let offset: usize = offset.try_into().unwrap();
                let length = db.stack.pop().unwrap();
                let length: usize = length.try_into().unwrap();
                let log_idx = u8::from(opcode) - u8::from(Opcode::LOG0);
                let mut topics = vec![];
                for i in 0..log_idx {
                    let topic = db.stack.pop().unwrap();
                    topics.push(topic);
                }
                db.logs.push(LogEvent {
                    data: db.memory.as_slice()[offset..offset+length].to_vec(),
                    topics: topics.clone(),
                });
                debug!("stack: {:0x?}", db.stack);
                debug!("OPCODE LOG{} - topics len: {:?}", log_idx.to_ascii_uppercase(), topics.len());
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

    impl Default for Context {
        fn default() -> Self {
            Self {
                caller: Default::default(),
                call_data: vec![],
                msg_value: Default::default(),
                chain_id: Default::default(),
            }
        }
    }

    impl Default for Db {
        fn default() -> Self {
            Self {
                stack: vec![],
                memory: vec![],
                storage: Default::default(),
                transient_storage: Default::default(),
                contract_offset: None,
                logs: vec![],
            }
        }
    }


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
        assert_eq!(*value_0, U256::from(36) + ctx.chain_id);

        Ok(())
    }

    #[test]
    #[traced_test]
    fn test_swap() -> Result<(), TinyEvmError> {
        let value_0 = 4;
        let value_1 = 42;
        let value_2 = 43;
        let bytecode_1 = [
            Opcode::PUSH1 as u8,
            value_0,
            Opcode::PUSH1 as u8,
            value_1,
            Opcode::PUSH1 as u8,
            value_2,
            Opcode::SWAP1 as u8,
            Opcode::STOP as u8,
        ];

        let bytecode_2 = [
            Opcode::PUSH1 as u8,
            value_0,
            Opcode::PUSH1 as u8,
            value_1,
            Opcode::PUSH1 as u8,
            value_2,
            Opcode::SWAP2 as u8,
            Opcode::STOP as u8,
        ];

        {
            let mut call_data = [0u8; 36];
            let mut db = Db::default();
            let mut ctx = Context::default();

            debug!("Exec bytecode_1:");
            let res_1 = exec_bytecode(bytecode_1.as_slice(), &mut db, &mut ctx);
            assert!(res_1.is_ok());
            assert_eq!(db.stack, vec![
                U256::from(value_0),
                U256::from(value_2),
                U256::from(value_1)
            ]);
        }

        {
            let mut db = Db::default();
            let mut ctx = Context::default();
            debug!("Exec bytecode_2:");
            let res_2 = exec_bytecode(bytecode_2.as_slice(), &mut db, &mut ctx);
            assert!(res_2.is_ok());
            assert_eq!(db.stack, vec![
                U256::from(value_2),
                U256::from(value_1),
                U256::from(value_0)
            ]);
        }

        Ok(())
    }

    #[test]
    #[traced_test]
    fn test_event() -> Result<(), TinyEvmError> {

        // TODO: what is the correct data to hash for a return?
        let call_data_hash: Vec<u8> = {
            let to_hash = "and_and_event(uint256)";
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

        let (db, ctx) = exec(Path::new("resources/output/AddAndEvent.bin"), call_data.as_slice(), None)?;

        // debug!("db logs: {:?}", db.logs);

        assert_eq!(U256::from_be_slice(db.logs[0].data.as_slice()), U256::from(1));
        assert_eq!(U256::from_be_slice(db.logs[1].data.as_slice()), U256::ZERO);
        assert_eq!(U256::from_be_slice(db.logs[2].data.as_slice()), U256::from(128));

        // TODO
        // let log_3_slice = db.logs[3].data.as_slice();
        // let log_3_len = &log_3_slice[0..32];

        let log_4_slice = db.logs[4].data.as_slice();
        // let log_4_len = &log_4_slice[0..32];
        let log_4_v1 = &log_4_slice[32..64];
        let log_4_v2 = &log_4_slice[64..96];

        // value of variable z == 128
        assert_eq!(U256::from_be_slice(log_4_v1), U256::from(128));
        let msg_expected = b"Hello World from MyLog2 !";
        // length(msg_expected)
        assert_eq!(U256::from_be_slice(log_4_v2), U256::from(msg_expected.len()));
        assert_eq!(&log_4_slice[96..96 + 25], msg_expected);

        Ok(())
    }

    #[test]
    #[traced_test]
    fn test_sha3() -> Result<(), TinyEvmError> {

        let msg = "Hello keccak256 !!";
        let mut hasher = Keccak256::new();
        hasher.update(msg.as_bytes());
        let result = hasher.finalize();
        let msg_hash = U256::from_be_slice(result.as_slice());
        
        let msg_bytes_len: [u8; 32] = U256::from(msg.as_bytes().len()).to_be_bytes();
        let mut msg = msg.as_bytes().to_vec();
        let len_with_padding = {
            let mut factor = msg.len() / 32;
            if msg.len() % 32 > 0 {
                factor += 1;
            }
            factor * 32
        };
        
        msg.resize(len_with_padding, 0);
        
        let msg_start: [u8; 32] = U256::from(32).to_be_bytes();
        
        let call_data_hash: Vec<u8> = {
            let to_hash = "hash(string)";
            let mut hasher = Keccak256::new();
            hasher.update(to_hash);
            let result = hasher.finalize();
            result.to_vec()
        };

        // From https://docs.soliditylang.org/en/v0.8.11/abi-spec.html
        // Call data structure:
        // 1- method id (4 bytes)
        // 2- offset where the first argument data starts
        // 3- len of string (length is a byte length and not a character length)
        // 4- string bytes (padded to 32  bytes)
        let call_data = call_data_hash
            .into_iter()
            .take(4)
            .chain(msg_start.into_iter())
            .chain(msg_bytes_len.into_iter())
            .chain(msg.into_iter())
            .collect::<Vec<u8>>();
        
        debug!("call_data (len: {}): {:0x?}", call_data.len(), call_data);

        // sha3.sol
        let (db, ctx) = exec(Path::new("resources/output/HashContract.bin"), call_data.as_slice(), None)?;
        
        assert_eq!(*db.storage.get(&U256::ZERO).unwrap(), msg_hash);
        
        Ok(())
    }
}