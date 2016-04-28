#![feature(slice_patterns)]

extern crate clap;
#[macro_use]
extern crate nom;

use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::u8;

use clap::{Arg, App};

#[derive(Debug)]
struct Addr(u8, u8);

impl ::std::fmt::Display for Addr {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        let Addr(hi, lo) = *self;
        write!(f, "${:>0pad$x}{:>0pad$x}", lo, hi, pad=2)
    }
}

/// An 8080 Register
#[derive(Debug)]
enum Reg {
    A, B, C, D, E, H, L
}

impl ::std::fmt::Display for Reg {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Reg::A => write!(f, "A"),
            Reg::B => write!(f, "B"),
            Reg::C => write!(f, "C"),
            Reg::D => write!(f, "D"),
            Reg::E => write!(f, "E"),
            Reg::H => write!(f, "H"),
            Reg::L => write!(f, "L")
        }
    }
}

#[derive(Debug)]
enum RegPair {
    BC,
    DE,
    HL,
    SP
}

impl ::std::fmt::Display for RegPair {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            RegPair::BC => write!(f, "B"),
            RegPair::DE => write!(f, "D"),
            RegPair::HL => write!(f, "H"),
            RegPair::SP => write!(f, "SP")
        }
    }
}

/// An 8080 Op code and its data
#[allow(non_camel_case_types)]
#[derive(Debug)]
enum Op {
    // MOV -> (dst, src)
    MOV_RR(Reg, Reg),
    MOV_MR(Reg),
    MOV_RM(Reg),

    // MVI -> (dst, data)
    MVI_RD(Reg, u8),
    MVI_M(u8),
    MVI_A(u8),

    // LXI -> (dst, data)
    LXI(RegPair, u8, u8),

    // LDA -> src
    LDA(Addr),

    // STA -> dst
    STA(Addr),

    // LHLD -> src
    LHLD(Addr),

    // SHLD -> dst
    SHLD(Addr),

    // LDAX -> src
    LDAX(RegPair),

    // STAX -> dst
    STAX(RegPair),

    XCHG,

    ADD_R(Reg),
    ADD_M,

    ADI(u8),

    ADC_R(Reg),
    ADC_M,

    ACI(u8),

    SUB_R(Reg),
    SUB_M,

    SUI(u8),

    SBB_R(Reg),
    SBB_M,

    SBI(u8),

    INR_R(Reg),
    INR_M,

    DCR_R(Reg),
    DCR_M,

    INX(RegPair),

    DCX(RegPair),

    DAD(RegPair),

    DAA,

    ANA_R(Reg),
    ANA_M,

    ANI(u8),

    XRA_R(Reg),
    XRA_M,

    XRI(u8),

    ORA_R(Reg),
    ORA_M,

    ORI(u8),

    CMP_R(Reg),
    CMP_M,

    CPI(u8),

    RLC,
    RRC,
    RAL,
    RAR,
    CMA,
    CMC,
    STC,

    RNC,
    JNC(Addr),
    JC(Addr),
    CNC(Addr),
    CC(Addr),
    RC,
    RPO,
    JPO(Addr),
    CPO(Addr),

    RPE,
    JPE(Addr),
    CPE(Addr),
    RP,
    JP(Addr),
    CP(Addr),

    JMP(Addr),
    CALL(Addr),
    RET,
    RST(u8),
    PCHL,

    RNZ,
    RZ,
    JNZ(Addr),
    JZ(Addr),
    CNZ(Addr),
    CZ(Addr),

    PUSH_SP(RegPair),
    PUSH_PSW,

    POP_SP(RegPair),
    POP_PSW,

    XTHL,
    SPHL,

    IN(u8),
    OUT(u8),

    EI,
    DI,
    HTL,

    NOP,

    SIM,
    RIM,
    RM,
    JM(Addr),
    CM(Addr),
}

impl Op {
    fn bytes(&self) -> u8 {
        match *self {
            Op::NOP => 1,
            Op::MOV_RR(_, _) => 3,
            Op::MOV_MR(_) => 2,
            Op::MOV_RM(_) => 2,
            Op::MVI_RD(_, _) => 3,
            Op::MVI_M(_) => 2,
            Op::MVI_A(_) => 2,
            Op::JMP(_) => 3,
            _ => 0
        }
    }
}

impl ::std::fmt::Display for Op {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Op::NOP => write!(f, "NOP"),
            Op::MOV_RR(ref dst, ref src) => write!(f, "MOV\t{},{}", dst, src),
            Op::MOV_MR(ref src) => write!(f, "MOV\tM,{}", src),
            Op::MOV_RM(ref dst) => write!(f, "MOV\t{},M", dst),
            Op::MVI_RD(ref dst, d) => write!(f, "MOV\t{},#{}", dst, d),
            Op::MVI_M(d) => write!(f, "MVI\tM,#${:>0pad$x}", d, pad=2),
            Op::MVI_A(d) => write!(f, "MVI\tA,#${:01$x}", d, 2),
            Op::JMP(ref addr) => write!(f, "JMP\t{}", addr),
            Op::PUSH_PSW => write!(f, "PUSH\tPSW"),
            Op::PUSH_SP(ref rp) => write!(f, "PUSH\t{}", rp),
            Op::POP_PSW => write!(f, "POP\tPSW"),
            Op::POP_SP(ref rp) => write!(f, "POP\t{}", rp),
            Op::STA(ref addr) => write!(f, "STA\t{}", addr),
            Op::LXI(ref dst, hi, lo) => write!(f, "LXI\t{},#${:>0pad$x}{:>0pad$x}", dst, hi, lo, pad=2),
            Op::DCR_M => write!(f, "DCR\tM"),
            Op::DCX(ref rp) => write!(f, "DCX\t{}", rp),
            Op::DAD(ref rp) => write!(f, "DAD\t{}", rp),
            Op::CALL(ref addr) => write!(f, "CALL\t{}", addr),
            Op::IN(port) => write!(f, "IN\t\t#${:>0pad$x}", port, pad=2),
            Op::OUT(port) => write!(f, "OUT\t#${:>0pad$x}", port, pad=2),
            Op::RRC => write!(f, "RRC"),
            Op::JC(ref addr) => write!(f, "JC\t\t{}", addr),
            Op::LDA(ref addr) => write!(f, "LDA\t{}", addr),
            Op::LDAX(ref rp) => write!(f, "LDAX\t{}", rp),
            Op::INX(ref rp) => write!(f, "INX\t{}", rp),
            Op::CPI(d) => write!(f, "CPI\t#${:>0pad$x}", d, pad=2),
            Op::ANA_R(ref reg) => write!(f, "ANA\t{}", reg),
            Op::JZ(ref addr) => write!(f, "JZ\t\t{}", addr),
            Op::JNZ(ref addr) => write!(f, "JNZ\t{}", addr),
            Op::JNC(ref addr) => write!(f, "JNC\t{}", addr),
            Op::ADD_R(ref reg) => write!(f, "ADD\t{}", reg),
            Op::ADD_M => write!(f, "ADD\tM"),
            Op::ADI(d) => write!(f, "ADI\t{:>0pad$x}", d, pad=2),
            Op::DAA => write!(f, "DAA"),
            Op::SHLD(ref addr) => write!(f, "SHLD\t{}", addr),
            Op::LHLD(ref addr) => write!(f, "LHLD\t{}", addr),
            Op::XRA_R(ref reg) => write!(f, "XRA\t{}", reg),
            // Op::SRA(ref reg) => write!("SRA\t{}", reg),
            _ => write!(f, "{:?}", self)
        }
    }
}

struct Program(Vec<Op>);

// impl ::std::fmt::Display for Program {
//     fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
//         let Program(ref ops) = *self;
//         for (i, op) in ops.iter().enumerate() {
//             write!(f, "{:>0pad$x} {}\n", i, op, pad=4)
//         }
//     }
// }

impl Program {
    pub fn write<W: Write>(&self, writer: &mut W) {
        let &Program(ref ops) = self;
        for (i, op) in ops.iter().enumerate() {
            writer.write(&format!("{:>0pad$x} {}\n", i, op, pad=4).into_bytes()).unwrap();
        }
    }
}

// TOOD: change data[x] to data.get_unchecked for performance
fn bytes_to_lxi_b(data: &[u8]) -> Op {
    Op::LXI(RegPair::BC, data[1], data[0])
}

fn bytes_to_lxi_d(data: &[u8]) -> Op {
    Op::LXI(RegPair::DE, data[1], data[0])
}

fn bytes_to_lxi_h(data: &[u8]) -> Op {
    Op::LXI(RegPair::HL, data[1], data[0])
}

fn bytes_to_lxi_sp(data: &[u8]) -> Op {
    Op::LXI(RegPair::SP, data[1], data[0])
}

fn bytes_to_mvi_b(data: &[u8]) -> Op {
    Op::MVI_RD(Reg::B, data[0])
}

fn bytes_to_mvi_c(data: &[u8]) -> Op {
    Op::MVI_RD(Reg::C, data[0])
}

fn bytes_to_mvi_d(data: &[u8]) -> Op {
    Op::MVI_RD(Reg::D, data[0])
}

fn bytes_to_mvi_e(data: &[u8]) -> Op {
    Op::MVI_RD(Reg::E, data[0])
}

fn bytes_to_mvi_h(data: &[u8]) -> Op {
    Op::MVI_RD(Reg::H, data[0])
}

fn bytes_to_mvi_l(data: &[u8]) -> Op {
    Op::MVI_RD(Reg::L, data[0])
}

fn bytes_to_mvi_m(data: &[u8]) -> Op {
    Op::MVI_M(data[0])
}

fn bytes_to_mvi_a(data: &[u8]) -> Op {
    Op::MVI_A(data[0])
}

fn bytes_to_shld(data: &[u8]) -> Op {
    Op::SHLD(Addr(data[0], data[1]))
}

fn bytes_to_lhld(data: &[u8]) -> Op {
    Op::LHLD(Addr(data[0], data[1]))
}

fn bytes_to_sta(data: &[u8]) -> Op {
    Op::STA(Addr(data[0], data[1]))
}

fn bytes_to_lda(data: &[u8]) -> Op {
    Op::LDA(Addr(data[0], data[1]))
}

fn bytes_to_jnz(data: &[u8]) -> Op {
    Op::JNZ(Addr(data[0], data[1]))
}

fn bytes_to_jz(data: &[u8]) -> Op {
    Op::JZ(Addr(data[0], data[1]))
}

fn bytes_to_jnc(data: &[u8]) -> Op {
    Op::JNC(Addr(data[0], data[1]))
}

fn bytes_to_jc(data: &[u8]) -> Op {
    Op::JC(Addr(data[0], data[1]))
}

fn bytes_to_jmp(data: &[u8]) -> Op {
    Op::JMP(Addr(data[0], data[1]))
}

fn bytes_to_cnz(data: &[u8]) -> Op {
    Op::CNZ(Addr(data[0], data[1]))
}

fn bytes_to_cnc(data: &[u8]) -> Op {
    Op::CNC(Addr(data[0], data[1]))
}

fn bytes_to_cc(data: &[u8]) -> Op {
    Op::CC(Addr(data[0], data[1]))
}

fn bytes_to_cz(data: &[u8]) -> Op {
    Op::CZ(Addr(data[0], data[1]))
}

fn bytes_to_cp(data: &[u8]) -> Op {
    Op::CP(Addr(data[0], data[1]))
}

fn bytes_to_adi(data: &[u8]) -> Op {
    Op::ADI(data[0])
}

fn bytes_to_aci(data: &[u8]) -> Op {
    Op::ACI(data[0])
}

fn bytes_to_ani(data: &[u8]) -> Op {
    Op::ANI(data[0])
}

fn bytes_to_call(data: &[u8]) -> Op {
    Op::CALL(Addr(data[0], data[1]))
}

fn bytes_to_out(data: &[u8]) -> Op {
    Op::OUT(data[0])
}

fn bytes_to_in(data: &[u8]) -> Op {
    Op::IN(data[0])
}

fn bytes_to_sui(data: &[u8]) -> Op {
    Op::SUI(data[0])
}

fn bytes_to_sbi(data: &[u8]) -> Op {
    Op::SBI(data[0])
}

fn bytes_to_jpo(data: &[u8]) -> Op {
    Op::JPO(Addr(data[0], data[1]))
}

fn bytes_to_jpe(data: &[u8]) -> Op {
    Op::JPE(Addr(data[0], data[1]))
}

fn bytes_to_jp(data: &[u8]) -> Op {
    Op::JP(Addr(data[0], data[1]))
}

fn bytes_to_jm(data: &[u8]) -> Op {
    Op::JM(Addr(data[0], data[1]))
}

fn bytes_to_cpo(data: &[u8]) -> Op {
    Op::CPO(Addr(data[0], data[1]))
}

fn bytes_to_cpe(data: &[u8]) -> Op {
    Op::CPE(Addr(data[0], data[1]))
}

fn bytes_to_cm(data: &[u8]) -> Op {
    Op::CM(Addr(data[0], data[1]))
}

fn bytes_to_xri(data: &[u8]) -> Op {
    Op::XRI(data[0])
}

fn bytes_to_ori(data: &[u8]) -> Op {
    Op::ORI(data[0])
}

fn bytes_to_cpi(data: &[u8]) -> Op {
    Op::CPI(data[0])
}

// named!(disassemble_lxi <Op>, tap!(data: take!(2) => value!(Op::LXI(RegPair::BC, data[0], data[1]))));

named!(disassemble(&[u8]) -> Vec<Op>,
    many1!(
        switch!(take!(1),
            [0x00] => value!(Op::NOP) |
            [0x01] => map!(take!(2), bytes_to_lxi_b) |
            [0x02] => value!(Op::STAX(RegPair::BC)) |
            [0x03] => value!(Op::INX(RegPair::BC)) |
            [0x04] => value!(Op::INR_R(Reg::B)) |
            [0x05] => value!(Op::DCR_R(Reg::B)) |
            [0x06] => map!(take!(1), bytes_to_mvi_b) |
            [0x07] => value!(Op::RLC) |
            // [0x08] => 
            [0x09] => value!(Op::DAD(RegPair::BC)) |
            [0x0a] => value!(Op::LDAX(RegPair::BC)) |
            [0x0b] => value!(Op::DCX(RegPair::BC)) |
            [0x0c] => value!(Op::INR_R(Reg::C)) |
            [0x0d] => value!(Op::DCR_R(Reg::C)) |
            [0x0e] => map!(take!(1), bytes_to_mvi_c) |
            [0x0f] => value!(Op::RRC) |
            // [0x10] =>
            [0x11] => map!(take!(2), bytes_to_lxi_d) |
            [0x12] => value!(Op::STAX(RegPair::DE)) |
            [0x13] => value!(Op::INX(RegPair::DE)) |
            [0x14] => value!(Op::INR_R(Reg::D)) |
            [0x15] => value!(Op::DCR_R(Reg::D)) |
            [0x16] => map!(take!(1), bytes_to_mvi_d) |
            [0x17] => value!(Op::RAL) |
            // [0x18] => 
            [0x19] => value!(Op::DAD(RegPair::DE)) |
            [0x1a] => value!(Op::LDAX(RegPair::DE)) |
            [0x1b] => value!(Op::DCX(RegPair::DE)) |
            [0x1c] => value!(Op::INR_R(Reg::E)) |
            [0x1d] => value!(Op::DCR_R(Reg::E)) |
            [0x1e] => map!(take!(1), bytes_to_mvi_e) |
            [0x1f] => value!(Op::RAR) |
            [0x20] => value!(Op::RIM) |
            [0x21] => map!(take!(2), bytes_to_lxi_h) |
            [0x22] => map!(take!(2), bytes_to_shld) |
            [0x23] => value!(Op::INX(RegPair::HL)) |
            [0x24] => value!(Op::INR_R(Reg::H)) |
            [0x25] => value!(Op::DCR_R(Reg::H)) |
            [0x26] => map!(take!(1), bytes_to_mvi_h) |
            [0x27] => value!(Op::DAA) |
            // [0x28] => 
            [0x29] => value!(Op::DAD(RegPair::HL)) |
            [0x2a] => map!(take!(2), bytes_to_lhld) |
            [0x2b] => value!(Op::DCX(RegPair::HL)) |
            [0x2c] => value!(Op::INR_R(Reg::L)) |
            [0x2d] => value!(Op::DCR_R(Reg::L)) |
            [0x2e] => map!(take!(1), bytes_to_mvi_l) |
            [0x2f] => value!(Op::CMA) |
            [0x30] => value!(Op::SIM) |
            [0x31] => map!(take!(2), bytes_to_lxi_sp) |
            [0x32] => map!(take!(2), bytes_to_sta) |
            [0x33] => value!(Op::INX(RegPair::SP)) |
            [0x34] => value!(Op::INR_M) |
            [0x35] => value!(Op::DCR_M) |
            [0x36] => map!(take!(1), bytes_to_mvi_m) |
            [0x37] => value!(Op::STC) |
            // [0x38] => 
            [0x39] => value!(Op::DAD(RegPair::SP)) |
            [0x3a] => map!(take!(2), bytes_to_lda) |
            [0x3b] => value!(Op::DCX(RegPair::SP)) |
            [0x3c] => value!(Op::INR_R(Reg::A)) |
            [0x3d] => value!(Op::DCR_R(Reg::A)) |
            [0x3e] => map!(take!(1), bytes_to_mvi_a) |
            [0x3f] => value!(Op::CMC) |
            [0x40] => value!(Op::MOV_RR(Reg::B, Reg::B)) |
            [0x41] => value!(Op::MOV_RR(Reg::B, Reg::C)) |
            [0x42] => value!(Op::MOV_RR(Reg::B, Reg::D)) |
            [0x43] => value!(Op::MOV_RR(Reg::B, Reg::E)) |
            [0x44] => value!(Op::MOV_RR(Reg::B, Reg::H)) |
            [0x45] => value!(Op::MOV_RR(Reg::B, Reg::L)) |
            [0x46] => value!(Op::MOV_RM(Reg::B)) |
            [0x47] => value!(Op::MOV_RR(Reg::B, Reg::A)) |
            [0x48] => value!(Op::MOV_RR(Reg::C, Reg::B)) |
            [0x49] => value!(Op::MOV_RR(Reg::C, Reg::C)) |
            [0x4a] => value!(Op::MOV_RR(Reg::C, Reg::D)) |
            [0x4b] => value!(Op::MOV_RR(Reg::C, Reg::E)) |
            [0x4c] => value!(Op::MOV_RR(Reg::C, Reg::H)) |
            [0x4d] => value!(Op::MOV_RR(Reg::C, Reg::L)) |
            [0x4e] => value!(Op::MOV_RM(Reg::C)) |
            [0x4f] => value!(Op::MOV_RR(Reg::C, Reg::A)) |
            [0x50] => value!(Op::MOV_RR(Reg::D, Reg::B)) |
            [0x51] => value!(Op::MOV_RR(Reg::D, Reg::C)) |
            [0x52] => value!(Op::MOV_RR(Reg::D, Reg::D)) |
            [0x53] => value!(Op::MOV_RR(Reg::D, Reg::E)) |
            [0x54] => value!(Op::MOV_RR(Reg::D, Reg::H)) |
            [0x55] => value!(Op::MOV_RR(Reg::D, Reg::L)) |
            [0x56] => value!(Op::MOV_RM(Reg::D)) |
            [0x57] => value!(Op::MOV_RR(Reg::D, Reg::A)) |
            [0x58] => value!(Op::MOV_RR(Reg::E, Reg::B)) |
            [0x59] => value!(Op::MOV_RR(Reg::E, Reg::C)) |
            [0x5a] => value!(Op::MOV_RR(Reg::E, Reg::D)) |
            [0x5b] => value!(Op::MOV_RR(Reg::E, Reg::E)) |
            [0x5c] => value!(Op::MOV_RR(Reg::E, Reg::H)) |
            [0x5d] => value!(Op::MOV_RR(Reg::E, Reg::L)) |
            [0x5e] => value!(Op::MOV_RM(Reg::E)) |
            [0x5f] => value!(Op::MOV_RR(Reg::E, Reg::A)) |
            [0x60] => value!(Op::MOV_RR(Reg::H, Reg::B)) |
            [0x61] => value!(Op::MOV_RR(Reg::H, Reg::C)) |
            [0x62] => value!(Op::MOV_RR(Reg::H, Reg::D)) |
            [0x63] => value!(Op::MOV_RR(Reg::H, Reg::E)) |
            [0x64] => value!(Op::MOV_RR(Reg::H, Reg::H)) |
            [0x65] => value!(Op::MOV_RR(Reg::H, Reg::L)) |
            [0x66] => value!(Op::MOV_RM(Reg::H)) |
            [0x67] => value!(Op::MOV_RR(Reg::H, Reg::A)) |
            [0x68] => value!(Op::MOV_RR(Reg::L, Reg::B)) |
            [0x69] => value!(Op::MOV_RR(Reg::L, Reg::C)) |
            [0x6a] => value!(Op::MOV_RR(Reg::L, Reg::D)) |
            [0x6b] => value!(Op::MOV_RR(Reg::L, Reg::E)) |
            [0x6c] => value!(Op::MOV_RR(Reg::L, Reg::H)) |
            [0x6d] => value!(Op::MOV_RR(Reg::L, Reg::L)) |
            [0x6e] => value!(Op::MOV_RM(Reg::L)) |
            [0x6f] => value!(Op::MOV_RR(Reg::L, Reg::A)) |
            [0x70] => value!(Op::MOV_MR(Reg::B)) |
            [0x71] => value!(Op::MOV_MR(Reg::C)) |
            [0x72] => value!(Op::MOV_MR(Reg::D)) |
            [0x73] => value!(Op::MOV_MR(Reg::E)) |
            [0x74] => value!(Op::MOV_MR(Reg::H)) |
            [0x75] => value!(Op::MOV_MR(Reg::L)) |
            [0x76] => value!(Op::HTL) |
            [0x77] => value!(Op::MOV_MR(Reg::A)) |
            [0x78] => value!(Op::MOV_RR(Reg::A, Reg::B)) |
            [0x79] => value!(Op::MOV_RR(Reg::A, Reg::C)) |
            [0x7a] => value!(Op::MOV_RR(Reg::A, Reg::D)) |
            [0x7b] => value!(Op::MOV_RR(Reg::A, Reg::E)) |
            [0x7c] => value!(Op::MOV_RR(Reg::A, Reg::H)) |
            [0x7d] => value!(Op::MOV_RR(Reg::A, Reg::L)) |
            [0x7e] => value!(Op::MOV_RM(Reg::A)) |
            [0x7f] => value!(Op::MOV_RR(Reg::A, Reg::A)) |
            [0x80] => value!(Op::ADD_R(Reg::B)) |
            [0x81] => value!(Op::ADD_R(Reg::C)) |
            [0x82] => value!(Op::ADD_R(Reg::D)) |
            [0x83] => value!(Op::ADD_R(Reg::E)) |
            [0x84] => value!(Op::ADD_R(Reg::H)) |
            [0x85] => value!(Op::ADD_R(Reg::L)) |
            [0x86] => value!(Op::ADD_M) |
            [0x87] => value!(Op::ADD_R(Reg::A)) |
            // ...
            [0x90] => value!(Op::SUB_R(Reg::B)) |
            [0x91] => value!(Op::SUB_R(Reg::C)) |
            [0x92] => value!(Op::SUB_R(Reg::D)) |
            [0x93] => value!(Op::SUB_R(Reg::E)) |
            [0x94] => value!(Op::SUB_R(Reg::H)) |
            [0x95] => value!(Op::SUB_R(Reg::L)) |
            [0x96] => value!(Op::SUB_M) |
            [0x97] => value!(Op::SUB_R(Reg::A)) |
            // ...
            [0xa0] => value!(Op::ANA_R(Reg::B)) |
            [0xa1] => value!(Op::ANA_R(Reg::C)) |
            [0xa2] => value!(Op::ANA_R(Reg::D)) |
            [0xa3] => value!(Op::ANA_R(Reg::E)) |
            [0xa4] => value!(Op::ANA_R(Reg::H)) |
            [0xa5] => value!(Op::ANA_R(Reg::L)) |
            [0xa6] => value!(Op::ANA_M) |
            [0xa7] => value!(Op::ANA_R(Reg::A)) |
            [0xa8] => value!(Op::XRA_R(Reg::B)) |
            [0xa9] => value!(Op::XRA_R(Reg::C)) |
            [0xaa] => value!(Op::XRA_R(Reg::D)) |
            [0xab] => value!(Op::XRA_R(Reg::E)) |
            [0xac] => value!(Op::XRA_R(Reg::H)) |
            [0xad] => value!(Op::XRA_R(Reg::L)) |
            [0xae] => value!(Op::XRA_M) |
            [0xaf] => value!(Op::XRA_R(Reg::A)) |
            [0xb0] => value!(Op::ORA_R(Reg::B)) |
            [0xb1] => value!(Op::ORA_R(Reg::C)) |
            [0xb2] => value!(Op::ORA_R(Reg::D)) |
            [0xb3] => value!(Op::ORA_R(Reg::E)) |
            [0xb4] => value!(Op::ORA_R(Reg::H)) |
            [0xb5] => value!(Op::ORA_R(Reg::L)) |
            [0xb6] => value!(Op::ORA_M) |
            [0xb7] => value!(Op::ORA_R(Reg::A)) |
            [0xb8] => value!(Op::CMP_R(Reg::B)) |
            [0xb9] => value!(Op::CMP_R(Reg::C)) |
            [0xba] => value!(Op::CMP_R(Reg::D)) |
            [0xbb] => value!(Op::CMP_R(Reg::E)) |
            [0xbc] => value!(Op::CMP_R(Reg::H)) |
            [0xbd] => value!(Op::CMP_R(Reg::L)) |
            [0xbe] => value!(Op::CMP_M) |
            [0xbf] => value!(Op::CMP_R(Reg::A)) |
            [0xc0] => value!(Op::RNZ) |
            [0xc1] => value!(Op::POP_SP(RegPair::BC)) |
            [0xc2] => map!(take!(2), bytes_to_jnz) |
            [0xc3] => map!(take!(2), bytes_to_jmp) |
            [0xc4] => map!(take!(2), bytes_to_cnz) |
            [0xc5] => value!(Op::PUSH_SP(RegPair::BC)) |
            [0xc6] => map!(take!(1), bytes_to_adi) |
            [0xc7] => value!(Op::RST(0)) |
            [0xc8] => value!(Op::RZ) |
            [0xc9] => value!(Op::RET) |
            [0xca] => map!(take!(2), bytes_to_jz) |
            // [0xcb] => 
            [0xcc] => map!(take!(2), bytes_to_cz) |
            [0xcd] => map!(take!(2), bytes_to_call) |
            [0xce] => map!(take!(1), bytes_to_aci) |
            [0xcf] => value!(Op::RST(1)) |
            [0xd0] => value!(Op::RNC) |
            [0xd1] => value!(Op::POP_SP(RegPair::DE)) |
            [0xd2] => map!(take!(2), bytes_to_jnc) |
            [0xd3] => map!(take!(1), bytes_to_out) |
            [0xd4] => map!(take!(2), bytes_to_cnc) |
            [0xd5] => value!(Op::PUSH_SP(RegPair::DE)) |
            [0xd6] => map!(take!(1), bytes_to_sui) |
            [0xd7] => value!(Op::RST(2)) |
            [0xd8] => value!(Op::RC) |
            // [0xd9] => 
            [0xda] => map!(take!(2), bytes_to_jc) |
            [0xdb] => map!(take!(1), bytes_to_in) |
            [0xdc] => map!(take!(2), bytes_to_cc) |
            // [0xdd] => 
            [0xde] => map!(take!(1), bytes_to_sbi) |
            [0xdf] => value!(Op::RST(3)) |
            [0xe0] => value!(Op::RPO) |
            [0xe1] => value!(Op::POP_SP(RegPair::HL)) |
            [0xe2] => map!(take!(2), bytes_to_jpo) |
            [0xe3] => value!(Op::XTHL) |
            [0xe4] => map!(take!(2), bytes_to_cpo) |
            [0xe5] => value!(Op::PUSH_SP(RegPair::HL)) |
            [0xe6] => map!(take!(1), bytes_to_ani) |
            [0xe7] => value!(Op::RST(4)) |
            [0xe8] => value!(Op::RPE) |
            [0xe9] => value!(Op::PCHL) |
            [0xea] => map!(take!(2), bytes_to_jpe) |
            [0xeb] => value!(Op::XCHG) |
            [0xec] => map!(take!(2), bytes_to_cpe) |
            // [0xed] =>
            [0xee] => map!(take!(1), bytes_to_xri) |
            [0xef] => value!(Op::RST(5)) |
            [0xf0] => value!(Op::RP) |
            [0xf1] => value!(Op::POP_PSW) |
            [0xf2] => map!(take!(2), bytes_to_jp) |
            [0xf3] => value!(Op::DI) |
            [0xf4] => map!(take!(2), bytes_to_cp) |
            [0xf5] => value!(Op::PUSH_PSW) |
            [0xf6] => map!(take!(1), bytes_to_ori) |
            [0xf7] => value!(Op::RST(6)) |
            [0xf8] => value!(Op::RM) |
            [0xf9] => value!(Op::SPHL) |
            [0xfa] => map!(take!(2), bytes_to_jm) |
            [0xfb] => value!(Op::EI) |
            [0xfc] => map!(take!(2), bytes_to_cm) |
            // [0xfd] => 
            [0xfe] => map!(take!(1), bytes_to_cpi) |
            [0xff] => value!(Op::RST(7))
        )
    )
);

struct Options {
    filename: String,
}

fn get_opts() -> Options {
    let matches = App::new("emu8080")
                       .version("0.1")
                       .arg(Arg::with_name("FILENAME")
                            .required(true))
                       .get_matches();
    let filename = matches.value_of("FILENAME").unwrap();
    Options {
        filename: String::from(filename),
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ConditionCodes {
    z: u8, // Zero
    s: u8, // Sign
    p: u8, // Parity
    cy: u8, // Carry
    ac: u8,
    pad: u8,
}

impl ConditionCodes {
    pub fn new() -> ConditionCodes {
        ConditionCodes {
            z: 0x00,
            s: 0x00,
            p: 0x00,
            cy: 0x00,
            ac: 0x00,
            pad: 0x00,
        }
    }

    #[inline(always)]
    pub fn update(&mut self, x: u16) {
        // If x is zero, set zero flag to 1, else 0.
        self.z = if x & 0xff == 0 { 1 } else { 0 };
        // Sign flag: if bit 7 is set, set the flag, else clear.
        self.s = if x & 0x80 > 0 { 1 } else { 0 };
        // Carry flag
        self.cy = if x > 0xff { 1 } else { 0 };               
        // Parity flag
        self.p = parity((x & 0xff) as u8, 8);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Memory(Vec<u8>);

impl Memory {
    pub fn new() -> Self {
        Self::with_size(65536) // 16 kB of Memory
    }

    pub fn with_size(size: usize) -> Self {
        let mut mem = Vec::with_capacity(size);
        mem.resize(size, 0); // zero the memory
        Memory(mem)
    }

    #[inline(always)]
    pub fn read(&self, i: u16) -> u8 {
        let Memory(ref mem) = *self;
        mem[i as usize]
    }

    #[inline(always)]
    pub fn write(&mut self, i: u16, d: u8) {
        let Memory(ref mut mem) = *self;
        mem[i as usize] = d;
    }
}

fn parity(x: u8, size: u8) -> u8 {
    let mut p = 0;
    let mut x = x & ((1u8.wrapping_shl(size as u32)) - 1);
    for _ in 0..size {
        if (x & 0x1) > 0 {
            p += 1;
        }
        x = x.wrapping_shr(1);
    }

    if (p & 0x1) == 0 {
        1
    } else {
        0
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Cpu {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
    mem: Memory,
    cc: ConditionCodes,
    interrupt_enabled: bool,
}

impl Cpu {
    pub fn new() -> Self {
        Self::with_size(65536)
    }

    pub fn with_size(size: usize) -> Self {
        Cpu {
            a: 0x00,
            b: 0x00,
            c: 0x00,
            d: 0x00,
            e: 0x00,
            h: 0x00,
            l: 0x00,
            sp: 0x0000,
            pc: 0x0000,
            mem: Memory::with_size(size),
            cc: ConditionCodes::new(),
            interrupt_enabled: false,
        }
    }

    /// Read a byte from memory at the address pointed to
    /// by PC and execute the OpCode given by that byte.
    pub fn emulate(&mut self) {
        let op = self.mem.read(self.pc);
        println!("pc: {:>0padpc$x}, op: {:>0padop$x}", self.pc, op, padpc=4, padop=2);
        match op {
            0x00 => {}, // NOP
            0x01 => { // LXI B,word
                self.c = self.mem.read(self.pc + 1);
                self.b = self.mem.read(self.pc + 2);
                self.pc += 2;
            },
            0x02 => { // STAX B
                let addr = (self.c as u16) << 8 | (self.b as u16);
                self.a = self.mem.read(addr);
            },
            0x03 => { // INX B
                let bc = (self.c as u16) << 8 | (self.b as u16);
                let bc = bc.wrapping_add(1);
                self.b = (bc & 0xff) as u8;
                self.c = ((bc & 0xff00) >> 8) as u8;
            },
            0x04 => { // INR B
                let b = (self.b as u16).wrapping_add(1);
                self.cc.update(b);
                self.b = (b & 0xff) as u8;
            },
            0x05 => { // DCR B
                let b = (self.b as u16).wrapping_sub(1);
                self.cc.update(b);
                self.b = (b & 0xff) as u8;
            },
            0x06 => {
                let x = self.mem.read(self.pc + 1);
                self.b = x;
                self.pc += 1;
            },
            0x09 => { // DAD B
                let bc = (self.b as u32) << 8 | (self.c as u32);
                let hl = (self.h as u32) << 8 | (self.l as u32);
                let hl = hl.wrapping_add(bc);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) > 0 { 1 } else { 0 };
            },
            0x0d => { // DCR C
                let mut c = self.c as u16;
                let c = (self.c as u16).wrapping_sub(1);
                self.cc.update(c);
                self.c = (c & 0xff) as u8;
            },
            0x0e => { // MVI C,D8
                let x = self.mem.read(self.pc + 1);
                self.c = x;
                self.pc += 1;
            },
            0x0f => { // RRC
                let x = self.a;
                self.a = x.rotate_right(1);
                // If register had a bit switched on in the LSB
                // then rotating right will have caused a carry.
                if x & 0x01 == 1 {
                    self.cc.cy = 1;
                }
            },
            0x11 => { // LXI D,D16
                let d = self.mem.read(self.pc + 2);
                let e = self.mem.read(self.pc + 1);
                self.d = d;
                self.e = e;
                self.pc += 2;
            },
            0x13 => { // INX D
                let mut de = (self.d as u16) << 8 | (self.e as u16);
                let de = de.wrapping_add(1);
                self.e = (de & 0xff) as u8;
                self.d = ((de & 0xff00) >> 8) as u8;
            },
            0x18 => {}, // Nothing?
            0x19 => { // DAD D
                let de = (self.d as u32) << 8 | (self.e as u32);
                let hl = (self.h as u32) << 8 | (self.l as u32);
                let hl = hl.wrapping_add(de);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) != 0 { 1 } else { 0 };
            },
            0x1a => { // LDAX D
                let addr = (self.d as u16) << 8 | (self.e as u16);
                self.a = self.mem.read(addr);
            },
            0x1b => { // DCX D
                let de = (self.d as u16) << 8 | (self.e as u16);
                let de = de.wrapping_sub(1);
                self.e = (de & 0xff) as u8;
                self.d = ((de & 0xff00) >> 8) as u8;
            },
            0x1f => { // RAR
                // Rotates the carry bit right
                // and combines it with the value
                // in register A as MSB.
                let x = self.a;
                self.a = (self.cc.cy << 7) | (x >> 1);
                if x & 0x01 == 1 {
                    self.cc.cy = 1;
                }
            },
            0x21 => { // LXI H,D16
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.l = lo;
                self.h = hi;
                self.pc += 2;
            },
            0x23 => { // INX H
                let hl = (self.h as u16) << 8 | (self.l as u16);
                let hl = hl.wrapping_add(1);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
            },
            0x24 => { // INR H
                let h = (self.h as u16).wrapping_add(1);
                self.cc.update(h);
                self.h = (h & 0xff) as u8;
            },
            0x26 => { // MVI H,D8
                let x = self.mem.read(self.pc + 1);
                self.h = x;
                self.pc += 1;
            },
            0x29 => { // DAD H
                let hl = (self.h as u32) << 8 | (self.l as u32);
                let hl = hl.wrapping_add(hl);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) != 0 { 1 } else { 0 };
            },
            0x31 => { // LXI SP,D16
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.sp = (hi as u16) << 8 | (lo as u16);
                self.pc += 2;
            },
            0x32 => { // STA addr
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                let addr = (hi as u16) << 8 | (lo as u16);
                self.mem.write(addr, self.a);
                self.pc += 2;
            },
            0x36 => { // MVI M,D8
                let x = self.mem.read(self.pc + 1);
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.mem.write(addr, x);
                self.pc += 1;
            },
            0x3a => { // LDA addr
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                let addr = (hi as u16) << 8 | (lo as u16);
                self.a = self.mem.read(addr);
                self.pc += 2;
            },
            0x3e => { // MVI A,D8
                let x = self.mem.read(self.pc + 1);
                self.a = x;
                self.pc += 1;
            },
            0x41 => self.b = self.c, // MOV B,C
            0x42 => self.b = self.d, // MOV B,D
            0x43 => self.b = self.e, // MOV B,E
            0x56 => { // MOV D,M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.d = self.mem.read(addr);
            },
            0x5e => { // MOV E,M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.e = self.mem.read(addr);
            },
            0x66 => { // MOV H,M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.h = self.mem.read(addr);
            },
            0x6f => self.l = self.a, // MOV L,A
            0x77 => { // MOV M,A
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.mem.write(addr, self.a);
            },
            0x7a => self.a = self.d, // MOV A,D
            0x7b => self.a = self.e, // MOV A,E
            0x7c => self.a = self.h, // MOV A,H
            0x7e => { // MOV A,M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.a = self.mem.read(addr);
            },
            0x80 => { // ADD B
                let result = self.a as u16 + self.b as u16;
                self.cc.update(result);
                // store result in register A
                self.a = (result & 0xff) as u8;
            },
            0xa7 => { // ANA A
                self.a = self.a & self.a;
                self.cc.cy = 0;
                self.cc.ac = 0;
                self.cc.z = if self.a == 0 { 1 } else { 0 };
                self.cc.s = if (self.a & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(self.a, 8);
            },
            0xaf => { // XRA A
                self.a = self.a ^ self.a;
                self.cc.cy = 0;
                self.cc.ac = 0;
                self.cc.z = if self.a == 0 { 1 } else { 0 };
                self.cc.s = if (self.a & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(self.a, 8);
            },
            0xc1 => { // POP B
                self.c = self.mem.read(self.sp);
                self.b = self.mem.read(self.sp + 1);
                self.sp += 2;
            },
            0xc2 => { // JNZ
                if self.cc.z == 0 {
                    let lo = self.mem.read(self.pc + 1);
                    let hi = self.mem.read(self.pc + 2);
                    self.pc = (hi as u16) << 8 | (lo as u16);
                    return;
                } else {
                    self.pc += 2;
                }
            },
            0xc3 => { // JMP addr
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.pc = (hi as u16) << 8 | (lo as u16);
                // Return here so we don't increment
                // the program counter at the end of the method.
                return;
            },
            0xc5 => { // PUSH B
                self.mem.write(self.sp - 1, self.b);
                self.mem.write(self.sp - 2, self.c);
                self.sp -= 2;
            },
            0xc6 => { // ADI D8
                let x = (self.a as u16) + self.mem.read(self.pc + 1) as u16;
                self.cc.update(x);
                self.a = x as u8;
                self.pc += 1;
            },
            0xc9 => { // RET
                let lo = self.mem.read(self.sp);
                let hi = self.mem.read(self.sp + 1);
                self.pc = (hi as u16) << 8 | (lo as u16);
                self.sp += 2;
                return;
            },
            0xcd => { // CALL addr
                // Save return address
                let ret = self.pc + 2;
                let lo = (ret & 0x00ff) as u8;
                let hi = ((ret & 0xff00) >> 8) as u8;
                self.mem.write(self.sp - 1, hi);
                self.mem.write(self.sp - 2, lo);
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.pc = (hi as u16) << 8 | (lo as u16);
                return;
            },
            0xd1 => { // POP D
                self.e = self.mem.read(self.sp);
                self.d = self.mem.read(self.sp + 1);
                self.sp += 2;
            },
            0xd3 => { // OUT D8
                // TODO: implement
                self.pc += 1;
            },
            0xd5 => { // PUSH D
                self.mem.write(self.sp - 1, self.d);
                self.mem.write(self.sp - 2, self.e);
                self.sp -= 2;
            },
            0xe1 => { // POP H
                self.l = self.mem.read(self.sp);
                self.h = self.mem.read(self.sp + 1);
                self.sp += 2;
            },
            0xe5 => { // PUSH H
                self.mem.write(self.sp - 1, self.h);
                self.mem.write(self.sp - 2, self.l);
                self.sp -= 2;
            },
            0xe6 => { // ANI D8
                let x = self.mem.read(self.pc + 1);
                self.a = self.a & x;
                self.cc.cy = 0;
                self.cc.ac = 0;
                self.cc.z = if self.a == 0 { 1 } else { 0 };
                self.cc.s = if (self.a & 0x80) == 0x80 { 1 } else { 0 };
                self.pc += 1;
            },
            0xeb => { // XCHG
                let d = self.d;
                let e = self.e;
                self.d = self.h;
                self.e = self.l;
                self.h = d;
                self.l = e;
            },
            0xf1 => { // POP PSW
                let x = self.mem.read(self.sp);
                self.cc.cy = x & 0b00000001;
                self.cc.p = (x >> 2) & 0x01;
                self.cc.ac = (x >> 4) & 0x01;
                self.cc.z = (x >> 6) & 0x01;
                self.cc.s = (x >> 7) & 0x01;
                self.a = self.mem.read(self.sp + 1);
                self.sp += 2;
            },
            0xf5 => { // PUSH PSW
                self.mem.write(self.sp - 1, self.a);
                let mut psw = 0b00000010;
                psw |= self.cc.cy;
                psw |= self.cc.p << 2;
                psw |= self.cc.ac << 4;
                psw |= self.cc.z << 6;
                psw |= self.cc.s << 7;
                self.mem.write(self.sp - 2, psw);
                self.sp -= 2;
            },
            0xfb => { // EI
                self.interrupt_enabled = true;
            },
            0xfe => { // CPI D8
                let x = self.mem.read(self.pc + 1);
                let y = self.a - x;
                self.cc.z = if y == 0 { 1 } else { 0 };
                self.cc.s = if y & 0x08 == 0x08 { 1 } else { 0 };
                self.cc.p = parity(x, 8);
                self.cc.cy = if self.a < x { 1 } else { 0 };
                self.pc += 1;
            },
            x => panic!("Unimplemented Op code: {:>0pad$x}", x, pad=2)
        }
        self.pc += 1;
    }

    // TODO: safe interface for reading/writing from/to memory.
    // This will allow the enforcement of a maximum memory size.
}

fn main() {
    let options = get_opts();
    let mut file = File::open(options.filename).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    let offset = 0;

    let mut cpu = Cpu::new();
    for (i, byte) in buf.iter().enumerate() {
        cpu.mem.write(i as u16 + offset, byte.clone());
    }

    loop {
        cpu.emulate();
    }

    // let program = match disassemble(&buf) {
    //     nom::IResult::Done(input, output) => Program(output),
    //     _ => panic!("Failed to parse input.")
    // };
    // io::stdout().write(&format!("{}", program).into_bytes()).unwrap();
    // program.write(&mut io::stdout());
}

#[test]
fn test_nop() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x00);
    cpu.emulate();
    let mut expected = Cpu::new();
    expected.pc += 1;
    assert_eq!(cpu, expected);
}

#[test]
fn test_lxi_b() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x01);
    cpu.mem.write(1, 0xfe);
    cpu.mem.write(2, 0xff);
    let mut expected = cpu.clone();
    expected.pc = 3;
    expected.b = 0xff;
    expected.c = 0xfe;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_stax_b() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x02);
    cpu.mem.write(0xfffe, 0xaa);
    cpu.b = 0xfe;
    cpu.c = 0xff;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0xfe;
    expected.c = 0xff;
    expected.a = 0xaa;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_inx_b() {
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x03);
    cpu.b = 0xfe;
    cpu.c = 0xff;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0xff;
    expected.c = 0xff;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_inr_b() {
    //! Test increment register B
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x04);
    cpu.b = 0x01;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x02;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_inr_b_carry_zero() {
    //! Test increment register B with Carry and Zero flag
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x04);
    cpu.b = 0xff;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x00;
    expected.cc.z = 1;
    expected.cc.cy = 1;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_dcr_b() {
    //! Test decrement register B
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x05);
    cpu.b = 0x02;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x01;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_dcr_b_zero() {
    //! Test decrement register B with Zero flag
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x05);
    cpu.b = 0x01;
    let mut expected = cpu.clone();
    expected.pc = 1;
    expected.b = 0x00;
    expected.cc.z = 1;

    cpu.emulate();
    assert_eq!(cpu, expected);
}

#[test]
fn test_mvi_b() {
    //! Test move immediate word to register B
    let mut cpu = Cpu::new();
    cpu.mem.write(0, 0x06);
    cpu.mem.write(1, 0xee);
    let mut expected = cpu.clone();
    expected.pc = 2;
    expected.b = 0xee;

    cpu.emulate();
    assert_eq!(cpu, expected);
}