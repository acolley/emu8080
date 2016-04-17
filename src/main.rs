#![feature(slice_patterns)]

#[macro_use]
extern crate nom;

#[derive(Debug)]
struct Addr(u16);

/// An 8080 Register
#[derive(Debug)]
enum Reg {
    A, B, C, D, E, H, L
}

#[derive(Debug)]
enum RegPair {
    BC,
    DE,
    HL,
    SP
}

/// An 8080 Op code and its data
#[allow(non_camel_case_types)]
#[derive(Debug)]
enum Op {
    // MOV -> (dst, src)
    MOV_RR(Reg, Reg),
    MOV_MR(Addr, Reg),
    MOV_RM(Reg, Addr),

    // MVI -> (dst, data)
    MVI_RD(Reg, u8),
    MVI_MD(Addr, u8),

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

    JMP(Addr),
    CALL(Addr),
    RET,
    RST(u8),
    PCHL,

    PUSH_RP(RegPair),
    PUSH_PSW,

    POP_RP(RegPair),
    POP_PSW,

    XTHL,
    SPHL,

    IN(u8),
    OUT(u8),

    EI,
    DI,
    HTL,

    NOP,
}

named!(disassemble <Op>,
    switch!(take!(1),
        [0x00] => value!(Op::NOP)
        // [0x01] => tap!(
        //     // data: take!(2) => value!(Op::LXI(RegPair::BC, data[0], data[1]))
        //     data: take!(2) => value!(Op::HTL)
        // )
    )
);

fn main() {
    println!("{:?}", disassemble(&[0x01]));
    println!("Hello, world!");
}
