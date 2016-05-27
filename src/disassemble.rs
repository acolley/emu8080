
use machine::{Reg, RegPair};

// /// A source/target location for an
// /// instruction.
// #[derive(Clone, Debug, PartialEq, Eq)]
// enum Loc {
//     Mem,
//     Reg(Reg),
// }

// #[derive(Clone, Debug, PartialEq, Eq)]
// pub enum OpCode {
//     NOP,
//     LXI(RegPair, u8, u8),
//     STAX(RegPair),
//     INX(RegPair),
//     INR(Reg),
//     DCR(Reg),
//     MVI(Reg, u8),
//     RLC,
//     RRC,
//     RAL,
//     RAR,
//     RIM,
//     DAD(RegPair),
//     LDAX(RegPair),
//     DCX(RegPair),
//     SHLD,
//     LHLD,
//     DAA,
//     CMA,
//     CMC,
//     STA,
//     STC,
//     MOV(Loc, Loc),
//     ADD(Loc),
//     SUB(Loc),
//     ANA(Loc),
//     XRA(Loc),
//     ORA(Loc),
//     CMP(Loc),
//     RNZ,
//     POP(RegPair),
// }

// impl OpCode {
//     /// The number of bytes this instruction uses.
//     pub fn bytes(&self) -> usize {
//         match *self {
//             NOP => 1,
//             LXI(_, _, _) => 3,
//             STAX(_) => 1,
//             INX(_) => 1,
//             INR(_) => 1,
//             DCR(_) => 1,
//             MVI(_, _) => 2,
//             RLC => 1,
//             RRC => 1,
//             RAL => 1,
//             RAR => 1,
//             RIM => 1
//             DAD(_) => 1,
//             LDAX(_) => 1,
//             DCX(_) => 1,
//             SHLD => 1,
//             LHLD => 1,
//             DAA => 1,
//             CMA => 1,
//             CMC => 1,
//             STA => 1,
//             STC => 1,
//             MOV(_, _) => 1,
//             ADD(_) => 1,
//             SUB(_) => 1,
//             ANA(_) => 1,
//             XRA(_) => 1,
//             ORA(_) => 1,
//             CMP(_) => 1,
//             RNZ => 1,
//             POP(_) => 1,
//         }
//     }
// }

// fn disassemble_op(bytes: &[u8], pc: usize) -> OpCode {
//     let op = bytes[pc];
//     match op {
//         0x00 => OpCode::NOP,
//         0x01 => {
//             let lo = bytes[pc + 1];
//             let hi = bytes[pc + 2];
//             OpCode::LXI(RegPair::BC, lo, hi)
//         },
//         0x02 => {
//             OpCode::STAX(RegPair::BC)
//         },
//         0x03 => {
//             OpCode::INX(RegPair::BC)
//         },
//         0x04 => {
//             OpCode::INR(RegPair::BC)
//         },
//         0x05 => {
//             OpCode::DCR(RegPair::BC)
//         },
//         0x06 => {
//             let x = bytes[pc + 1];
//             OpCode::MVI(Reg::B, x)
//         },
//     }
// }

/// Disassemble the given compiled 8080 binary
/// code in `bytes`.
/// `offset` is where the first instruction
/// starts in memory.
pub fn disassemble(bytes: &[u8], offset: usize) {
    let mut pc = 0;
    let mut iter = bytes.iter();
    while let Some(op) = iter.next() {
        print!("{:>0pad$x} ", pc + offset, pad=4);
        match *op {
            0x00 => println!("NOP"),
            0x01 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("LXI B #${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0x02 => println!("STAX B"),
            0x03 => println!("INX B"),
            0x04 => println!("INR B"),
            0x05 => println!("DCR B"),
            0x06 => {
                let x = iter.next().unwrap();
                println!("MVI B,#{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0x07 => println!("RLC"),
            // 0x08 =>
            0x09 => println!("DAD B"),
            0x0a => println!("LDAX B"),
            0x0b => println!("DCX B"),
            0x0c => println!("INR C"),
            0x0d => println!("DCR C"),
            0x0e => {
                let x = iter.next().unwrap();
                println!("MVI C,#{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0x0f => println!("RRC"),
            // 0x10 =>
            0x11 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("LXI D,#${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0x12 => println!("STAX D"),
            0x13 => println!("INX D"),
            0x14 => println!("INR D"),
            0x15 => println!("DCR D"),
            0x16 => {
                let x = iter.next().unwrap();
                println!("MVI D,#{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0x17 => println!("RAL"),
            // 0x18 => 
            0x19 => println!("DAD D"),
            0x1a => println!("LDAX D"),
            0x1b => println!("DCX D"),
            0x1c => println!("INR E"),
            0x1d => println!("DCR E"),
            0x1f => println!("RAR"),
            0x20 => println!("RIM"),
            0x21 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("LXI H,#{:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0x22 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("SHLD ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            }
            0x23 => println!("INX H"),
            0x24 => println!("INR H"),
            0x25 => println!("DCR H"),
            0x26 => {
                let x = iter.next().unwrap();
                println!("MVI H,#{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0x27 => println!("DAA"),
            0x29 => println!("DAD H"),
            0x2a => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("LHLD ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0x2b => println!("DCX H"),
            0x2c => println!("INR L"),
            0x2e => {
                let x = iter.next().unwrap();
                println!("MVI L,#{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0x2f => println!("CMA"),
            0x30 => println!("SIM"),
            0x31 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("LXI SP,#{:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0x32 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("STA ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0x34 => println!("INR M"),
            0x35 => println!("DCR M"),
            0x36 => {
                let x = iter.next().unwrap();
                println!("MVI M,#{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0x37 => println!("STC"),
            0x3a => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("LDA ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0x3c => println!("INR A"),
            0x3d => println!("DCR A"),
            0x3e =>  {
                let x = iter.next().unwrap();
                println!("MVI A,#{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0x3f => println!("CMC"),
            0x40 => println!("MOV B,B"),
            0x41 => println!("MOV B,C"),
            0x42 => println!("MOV B,D"),
            0x43 => println!("MOV B,E"),
            0x44 => println!("MOV B,H"),
            0x45 => println!("MOV B,L"),
            0x46 => println!("MOV B,M"),
            0x47 => println!("MOV B,A"),
            0x49 => println!("MOV C,C"),
            0x4d => println!("MOV C,L"),
            0x4e => println!("MOV C,M"),
            0x4f => println!("MOV C,A"),
            0x50 => println!("MOV D,B"),
            0x51 => println!("MOV D,C"),
            0x52 => println!("MOV D,D"),
            0x53 => println!("MOV D,E"),
            0x54 => println!("MOV D,H"),
            0x55 => println!("MOV D,L"),
            0x56 => println!("MOV D,M"),
            0x57 => println!("MOV D,A"),
            0x58 => println!("MOV E,B"),
            0x59 => println!("MOV E,C"),
            0x5a => println!("MOV E,D"),
            0x5b => println!("MOV E,E"),
            0x5c => println!("MOV E,H"),
            0x5d => println!("MOV E,L"),
            0x5e => println!("MOV E,M"),
            0x5f => println!("MOV E,A"),
            0x61 => println!("MOV H,C"),
            0x66 => println!("MOV H,M"),
            0x67 => println!("MOV H,A"),
            0x68 => println!("MOV L,B"),
            0x69 => println!("MOV L,C"),
            0x6f => println!("MOV L,A"),
            0x70 => println!("MOV M,B"),
            0x71 => println!("MOV M,C"),
            0x72 => println!("MOV M,D"),
            0x73 => println!("MOV M,E"),
            0x77 => println!("MOV M,A"),
            0x78 => println!("MOV A,B"),
            0x79 => println!("MOV A,C"),
            0x7a => println!("MOV A,D"),
            0x7b => println!("MOV A,E"),
            0x7c => println!("MOV A,H"),
            0x7d => println!("MOV A,L"),
            0x7e => println!("MOV A,M"),
            0x7f => println!("MOV A,A"),
            0x80 => println!("ADD B"),
            0x81 => println!("ADD C"),
            0x85 => println!("ADD L"),
            0x86 => println!("ADD M"),
            0x97 => println!("SUB A"),
            0xa0 => println!("ANA B"),
            0xa6 => println!("ANA M"),
            0xa7 => println!("ANA A"),
            0xa8 => println!("XRA B"),
            0xaf => println!("XRA A"),
            0xb0 => println!("ORA B"),
            0xb4 => println!("ORA H"),
            0xb6 => println!("ORA M"),
            0xb8 => println!("CMP B"),
            0xbc => println!("CMP H"),
            0xbe => println!("CMP M"),
            0xc0 => println!("RNZ"),
            0xc1 => println!("POP B"),
            0xc2 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JNZ ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xc3 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JMP ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xc4 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("CNZ ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            }
            0xc5 => println!("PUSH B"),
            0xc6 => {
                let x = iter.next().unwrap();
                println!("ADI #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xc8 => println!("RZ"),
            0xc9 => println!("RET"),
            0xca => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JZ ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xcc => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("CZ ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xcd => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("CALL ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xd0 => println!("RNC"),
            0xd1 => println!("POP D"),
            0xd2 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JNC ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xd3 => {
                let x = iter.next().unwrap();
                println!("OUT #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xd4 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("CNC ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xd5 => println!("PUSH D"),
            0xd6 => {
                let x = iter.next().unwrap();
                println!("SUI #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xda => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JC ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            }
            0xdb => {
                let x = iter.next().unwrap();
                println!("IN #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xde => {
                let x = iter.next().unwrap();
                println!("SBI #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xe1 => println!("POP H"),
            0xe2 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JPO ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xe3 => println!("XTHL"),
            0xe5 => println!("PUSH H"),
            0xe6 => {
                let x = iter.next().unwrap();
                println!("ANI #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xe9 => println!("PCHL"),
            0xea => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JPE ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xeb => println!("XCHG"),
            0xf1 => println!("POP PSW"),
            0xf2 => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JP ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xf5 => println!("PUSH PSW"),
            0xf6 => {
                let x = iter.next().unwrap();
                println!("ORI #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xf7 => println!("RST 6"),
            0xfa => {
                let lo = iter.next().unwrap();
                let hi = iter.next().unwrap();
                println!("JM ${:>0pad$x}{:>0pad$x}", hi, lo, pad=2);
                pc += 2;
            },
            0xfb => println!("EI"),
            0xfe => {
                let x = iter.next().unwrap();
                println!("CPI #{:>0pad$x}", x, pad=2);
                pc += 1;
            },
            0xff => println!("RST 7"),
            x => println!("unknown {:>0pad$x}", x, pad=2)
        }
        pc += 1;
    }
}