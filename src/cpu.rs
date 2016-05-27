
use std::mem;

use memory::Memory;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionCodes {
    pub z: u8, // Zero
    pub s: u8, // Sign
    pub p: u8, // Parity
    pub cy: u8, // Carry
    pub ac: u8,
    pub pad: u8,
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
}

const PARITY_BYTES: [u8; 8] = [
    0b00000001,
    0b00000010,
    0b00000100,
    0b00001000,
    0b00010000,
    0b00100000,
    0b01000000,
    0b10000000
];

/// If the number of 1's in a byte is
/// even return 1, otherwise return 0.
#[inline(always)]
fn parity(x: u8) -> u8 {
    let p = PARITY_BYTES.iter()
        .enumerate()
        .fold(0, |acc, ib| acc + ((ib.1 & x) >> ib.0));
    if (p % 2) == 0 { 1 } else { 0 }
}

/// Deconstruct a u16 value into its
/// component (lo, hi) bytes.
#[inline(always)]
fn split_u16(x: u16) -> (u8, u8) {
    let lo = (x & 0x00ff) as u8;
    let hi = ((x & 0xff00) >> 8) as u8;
    (lo, hi)
}

#[inline(always)]
pub fn make_u16(lo: u8, hi: u8) -> u16 {
    (hi as u16) << 8 | lo as u16
}

#[inline(always)]
fn make_u32(lo: u8, hi: u8) -> u32 {
    (hi as u32) << 8 | lo as u32
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cpu {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub sp: u16,
    pub pc: u16,
    pub mem: Memory,
    pub cc: ConditionCodes,
    pub interrupt_enabled: bool,
    pub ports: [u8; 8],
    pub speed: usize, // Clock speed in Hz
}

impl Cpu {
    pub fn new(memory: Memory) -> Self {
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
            mem: memory,
            cc: ConditionCodes::new(),
            interrupt_enabled: false,
            ports: [0; 8],
            speed: 2_000_000,
        }
    }

    #[inline(always)]
    pub fn bc(&self) -> u16 {
        make_u16(self.c, self.b)
    }

    #[inline(always)]
    pub fn set_bc(&mut self, x: u16) {
        let (lo, hi) = split_u16(x);
        self.b = hi;
        self.c = lo;
    }

    #[inline(always)]
    pub fn de(&self) -> u16 {
        make_u16(self.e, self.d)
    }

    #[inline(always)]
    pub fn set_de(&mut self, x: u16) {
        let (lo, hi) = split_u16(x);
        self.d = hi;
        self.e = lo;
    }

    #[inline(always)]
    pub fn hl(&self) -> u16 {
        make_u16(self.l, self.h)
    }

    #[inline(always)]
    pub fn set_hl(&mut self, x: u16) {
        let (lo, hi) = split_u16(x);
        self.h = hi;
        self.l = lo;
    }

    #[inline(always)]
    pub fn push(&mut self, lo: u8, hi: u8) {
        self.mem.write(self.sp - 2, lo);
        self.mem.write(self.sp - 1, hi);
        self.sp -= 2;
    }

    /// Pop (lo, hi) from stack and increment
    /// SP as appropriate.
    #[inline(always)]
    pub fn pop(&mut self) -> (u8, u8) {
        let lo = self.mem.read(self.sp);
        let hi = self.mem.read(self.sp + 1);
        self.sp += 2;
        (lo, hi)
    }

    #[inline(always)]
    fn inr(&mut self, x: u8) -> u8 {
        let x = x.wrapping_add(1);
        self.set_flags_zsp(x);
        self.cc.ac = if x & 0x0f == 0 { 1 } else { 0 };
        x
    }

    /// Decrement the given value setting
    /// the appropriate flags and returning
    /// the resultant value.
    #[inline(always)]
    fn dcr(&mut self, x: u8) -> u8 {
        let x = x.wrapping_sub(1);
        self.set_flags_zsp(x);
        self.cc.ac = if x & 0x0f == 0 { 1 } else { 0 };
        x
    }

    #[inline(always)]
    fn stax(&mut self, addr: u16) {
        let x = self.a;
        self.mem.write(addr, x);
    }

    #[inline(always)]
    fn add(&mut self, x: u8) {
        let a = self.a as u16 + x as u16;
        self.set_flags_arith(a);
        self.a = a as u8;
    }

    #[inline(always)]
    fn adc(&mut self, x: u8) {
        let a = self.a as u16 + x as u16 + self.cc.cy as u16;
        self.set_flags_arith(a);
        self.a = a as u8;
    }

    #[inline(always)]
    fn sub(&mut self, x: u8) {
        let a = self.a as u16 - x as u16;
        self.set_flags_arith(a);
        self.a = a as u8;
    }

    #[inline(always)]
    fn sbb(&mut self, x: u8) {
        let a = self.a as u16 - x as u16 - self.cc.cy as u16;
        self.set_flags_arith(a);
        self.a = a as u8;
    }

    #[inline(always)]
    fn ana(&mut self, x: u8) {
        let a = self.a & x;
        self.set_flags_logic(a);
        self.a = a;
    }

    #[inline(always)]
    fn xra(&mut self, x: u8) {
        let a = self.a ^ x;
        self.set_flags_logic(a);
        self.a = a;
    }

    #[inline(always)]
    fn ora(&mut self, x: u8) {
        let a = self.a | x;
        self.set_flags_logic(a);
        self.a = a;
    }

    #[inline(always)]
    fn cmp(&mut self, x: u8) {
        let a = self.a as u16 - x as u16;
        self.set_flags_arith(a);
    }

    /// CALL instruction
    #[inline(always)]
    fn call(&mut self) {
        // Push return address on to stack
        let ret = self.pc + 3;
        let (lo, hi) = split_u16(ret);
        self.push(lo, hi);
        self.pc = self.read_u16();
    }

    /// RET instruction
    #[inline(always)]
    fn ret(&mut self) {
        let (lo, hi) = self.pop();
        self.pc = make_u16(lo, hi);
    }

    /// Read a data byte and increment PC.
    #[inline(always)]
    fn read_byte(&mut self) -> u8 {
        let x = self.mem.read(self.pc + 1);
        self.pc += 1;
        x
    }

    /// Read (lo, hi) bytes and increment PC.
    #[inline(always)]
    fn read_two_bytes(&mut self) -> (u8, u8) {
        let lo = self.mem.read(self.pc + 1);
        let hi = self.mem.read(self.pc + 2);
        self.pc += 2;
        (lo, hi)
    }

    #[inline(always)]
    pub fn read_u16(&mut self) -> u16 {
        let lo = self.mem.read(self.pc + 1);
        let hi = self.mem.read(self.pc + 2);
        self.pc += 2;
        make_u16(lo, hi)
    }

    /// Read from the memory address given
    /// by the bytes held in the H & L registers.
    #[inline(always)]
    fn read_from_hl(&self) -> u8 {
        let addr = self.hl();
        self.mem.read(addr)
    }

    #[inline(always)]
    fn set_flags_zsp(&mut self, x: u8) {
        self.cc.z = if x == 0 { 1 } else { 0 };
        self.cc.s = if (x & 0x80 == 0x80) { 1 } else { 0 };
        self.cc.p = parity(x);
    }

    #[inline(always)]
    fn set_flags_arith(&mut self, x: u16) {
        self.set_flags_zsp(x as u8);
        self.cc.cy = if x > 0xff { 1 } else { 0 };
        // TODO: calculate Auxiliary Carry/Half Carry
        // https://en.wikipedia.org/wiki/Half-carry_flag
        // self.cc.ac = 
        self.cc.ac = 0;
    }

    #[inline(always)]
    fn set_flags_logic(&mut self, x: u8) {
        self.set_flags_zsp(x);
        self.cc.cy = 0;
    }

    /// Read a byte from memory at the address pointed to
    /// by PC and execute the OpCode given by that byte.
    /// Return the number of clock cycles used by that op.
    #[inline(always)]
    pub fn step(&mut self) -> u32 {
        let op = self.mem.read(self.pc);
        let cycles = match op {
            0x00 => { 4 }, // NOP
            0x01 => { // LXI B,word
                let (c, b) = self.read_two_bytes();
                self.c = c;
                self.b = b;
                10
            },
            0x02 => { // STAX B
                let addr = self.bc();
                self.stax(addr);
                7
            },
            0x03 => { // INX B
                self.c = self.c.wrapping_add(1);
                if self.c == 0 {
                    self.b = self.b.wrapping_add(1);
                }
                5
            },
            0x04 => { // INR B
                let b = self.b.wrapping_add(1);
                self.set_flags_zsp(b);
                self.b = b;
                5
            },
            0x05 => { // DCR B
                let b = self.b.wrapping_sub(1);
                self.set_flags_zsp(b);
                self.b = b;
                5
            },
            0x06 => { // MVI B,D8
                self.b = self.read_byte();
                7
            },
            0x07 => { // RLC
                self.a = self.a.rotate_left(1);
                // If register has a bit switched on in the LSB
                // then rotating left will have caused a carry.
                if self.a & 1 == 1 {
                    self.cc.cy = 1;
                }
                4
            },
            0x09 => { // DAD B
                let bc = make_u32(self.c, self.b);
                let hl = make_u32(self.l, self.h);
                let hl = hl.wrapping_add(bc);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) > 0 { 1 } else { 0 };
                10
            },
            0x0a => { // LDAX B
                let addr = self.bc();
                self.a = self.mem.read(addr);
                7
            },
            0x0b => { // DCX B
                self.c = self.c.wrapping_sub(1);
                if self.c == 0xff {
                    self.b = self.b.wrapping_sub(1);
                }
                5
            },
            0x0c => { // INR C
                let c = self.c.wrapping_add(1);
                self.set_flags_zsp(c);
                self.c = c;
                5
            },
            0x0d => { // DCR C
                let c = self.c.wrapping_sub(1);
                self.set_flags_zsp(c);
                self.c = c;
                5
            },
            0x0e => { // MVI C,D8
                self.c = self.read_byte();
                7
            },
            0x0f => { // RRC
                self.a = self.a.rotate_right(1);
                // If register has a bit switched on in the MSB
                // then rotating right will have caused a carry.
                if self.a & 0x80 == 0x80 {
                    self.cc.cy = 1;
                }
                4
            },
            0x11 => { // LXI D16,D16
                let (e, d) = self.read_two_bytes();
                self.d = d;
                self.e = e;
                10
            },
            0x12 => { // STAX D
                let addr = self.de();
                self.stax(addr);
                7
            },
            0x13 => { // INX D
                self.e = self.e.wrapping_add(1);
                if self.e == 0 {
                    self.d = self.d.wrapping_add(1);
                }
                5
            },
            0x14 => { // INR D
                let d = self.d.wrapping_add(1);
                self.set_flags_zsp(d);
                self.d = d;
                5
            },
            0x15 => { // DCR D
                let d = self.d.wrapping_sub(1);
                self.set_flags_zsp(d);
                self.d = d;
                5
            },
            0x16 => { // MVI D,D8
                self.d = self.read_byte();
                7
            },
            0x19 => { // DAD D
                let de = make_u32(self.e, self.d);
                let hl = make_u32(self.l, self.h);
                let hl = hl.wrapping_add(de);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) != 0 { 1 } else { 0 };
                10
            },
            0x1a => { // LDAX D
                let addr = self.de();
                self.a = self.mem.read(addr);
                7
            },
            0x1b => { // DCX D
                self.e = self.e.wrapping_sub(1);
                if self.e == 0xff {
                    self.d = self.d.wrapping_sub(1);
                }
                5
            },
            0x1c => { // INR E
                let e = self.e.wrapping_add(1);
                self.set_flags_zsp(e);
                self.e = e;
                5
            },
            0x1d => { // DCR E
                let e = self.e.wrapping_sub(1);
                self.set_flags_zsp(e);
                self.e = e;
                5
            },
            0x1e => { // MVI E,D8
                self.e = self.read_byte();
                7
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
                4
            },
            0x21 => { // LXI H,D16
                let (lo, hi) = self.read_two_bytes();
                self.l = lo;
                self.h = hi;
                10
            },
            0x22 => { // SHLD addr
                let addr = self.read_u16();
                self.mem.write(addr, self.l);
                self.mem.write(addr + 1, self.h);
                16
            },
            0x23 => { // INX H
                self.l = self.l.wrapping_add(1);
                if self.l == 0 {
                    self.h = self.h.wrapping_add(1);
                }
                5
            },
            0x24 => { // INR H
                let h = self.h.wrapping_add(1);
                self.set_flags_zsp(h);
                self.h = h;
                5
            },
            0x25 => { // DCR H
                let h = self.h.wrapping_sub(1);
                self.set_flags_zsp(h);
                self.h = h;
                5
            },
            0x26 => { // MVI H,D8
                self.h = self.read_byte();
                7
            },
            0x27 => { // DAA
                // This converts base 16 to base 10.
                // The eight-bit number in the accumulator is adjusted 
                // to form two four-bit Binary-Coded-Decimal digits 
                // by the following Process:
                // 1. If the value of the least significant 4 bits of the 
                // accumulator is greater than 9 OR if the AC flag is set, 
                // 6 is added to the accumulator.
                // 2. If the value of the most significant 4 bits of the 
                // accumulator is now greater than 9, or if the CY flag is set, 
                // 6 is added to the most significant 4 bits of the accumulator.
                let mut a = self.a as u16;
                if a & 0x0f > 9 || self.cc.ac == 1 {
                    a += 6;
                    self.cc.ac = 1;
                } else {
                    self.cc.ac = 0;
                }
                if a & 0xf0 > 0x90 || self.cc.cy == 1 {
                    a += 0x60;
                    self.cc.cy = 1;
                } else {
                    self.cc.cy = 0;
                }
                self.set_flags_zsp(a as u8);
                self.a = a as u8;
                4
            },
            0x29 => { // DAD H
                let hl = make_u32(self.l, self.h);
                let hl = hl.wrapping_add(hl);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) != 0 { 1 } else { 0 };
                10
            },
            0x2a => { // LHLD addr
                let (lo, hi) = self.read_two_bytes();
                let addr = make_u16(lo, hi);
                self.l = self.mem.read(addr);
                self.h = self.mem.read(addr + 1);
                16
            },
            0x2b => { // DCX H
                self.l = self.l.wrapping_sub(1);
                if self.l == 0xff {
                    self.h = self.h.wrapping_sub(1);
                }
                5
            },
            0x2c => { // INR L
                let l = self.l.wrapping_add(1);
                self.set_flags_zsp(l);
                self.l = l;
                5
            },
            0x2d => { // DCR L
                let l = self.l.wrapping_sub(1);
                self.set_flags_zsp(l);
                self.l = l;
                5
            },
            0x2e => { // MVI L,D8
                self.l = self.read_byte();
                7
            },
            0x2f => { // CMA
                self.a = !self.a;
                4
            },
            0x31 => { // LXI SP,D16
                self.sp = self.read_u16();
                10
            },
            0x32 => { // STA addr
                let addr = self.read_u16();
                self.mem.write(addr, self.a);
                13
            },
            0x34 => { // INR M
                let addr = self.hl();
                let x = self.mem.read(addr).wrapping_add(1);
                self.set_flags_zsp(x);
                self.mem.write(addr, x);
                10
            },
            0x35 => { // DCR M
                let addr = self.hl();
                let x = self.mem.read(addr).wrapping_sub(1);
                self.set_flags_zsp(x);
                self.mem.write(addr, x);
                10
            },
            0x36 => { // MVI M,D8
                let x = self.read_byte();
                let addr = self.hl();
                self.mem.write(addr, x);
                10
            },
            0x37 => { // STC
                self.cc.cy = 1;
                4
            },
            0x39 => { // DAD SP
                let hl = make_u32(self.l, self.h);
                let sp = (self.sp as u32).wrapping_add(hl);
                self.sp = (sp & 0x0000ffff) as u16;
                self.cc.cy = if (sp & 0xffff0000) != 0 { 1 } else { 0 };
                10
            },
            0x3a => { // LDA addr
                let addr = self.read_u16();
                self.a = self.mem.read(addr);
                13
            },
            0x3c => { // INR A
                let a = self.a.wrapping_add(1);
                self.set_flags_zsp(a);
                self.a = a;
                5
            },
            0x3d => { // DCR A
                let a = self.a.wrapping_sub(1);
                self.set_flags_zsp(a);
                self.a = a;
                5
            },
            0x3e => { // MVI A,D8
                self.a = self.read_byte();
                7
            },
            0x3f => { // CMC
                self.cc.cy = !self.cc.cy;
                4
            },
            0x41 => {  // MOV B,C
                self.b = self.c;
                5
            },
            0x42 => { // MOV B,D
                self.b = self.d;
                5
            },
            0x43 => { // MOV B,E
                self.b = self.e;
                5
            },
            0x44 => { // MOV B,H
                self.b = self.h;
                5
            },
            0x45 => { // MOV B,L
                self.b = self.l;
                5
            },
            0x46 => { // MOV B,M
                self.b = self.read_from_hl();
                7
            },
            0x47 => { // MOV B,A
                self.b = self.a;
                5
            },
            0x48 => { // MOV C,B
                self.c = self.b;
                5
            },
            0x49 => { // MOV C,C
                5
            },
            0x4a => { // MOV C,D
                self.c = self.d;
                5
            },
            0x4b => { // MOV C,E
                self.c = self.e;
                5
            },
            0x4c => { // MOV C,H
                self.c = self.h;
                5
            },
            0x4d => { // MOV C,L
                self.c = self.l;
                5
            },
            0x4e => { // MOV C,M
                self.c = self.read_from_hl();
                7
            },
            0x4f => { // MOV C,A
                self.c = self.a;
                5
            },
            0x50 => { // MOV D,B
                self.d = self.b;
                5
            },
            0x51 => { // MOV D,C
                self.d = self.c;
                5
            },
            0x52 => { // MOV D,D
                5
            },
            0x53 => { // MOV D,E
                self.d = self.e;
                5
            },
            0x54 => { // MOV D,H
                self.d = self.h;
                5
            },
            0x55 => { // MOV D,L
                self.d = self.l;
                5
            },
            0x56 => { // MOV D,M
                self.d = self.read_from_hl();
                7
            },
            0x57 => { // MOV D,A
                self.d = self.a;
                5
            },
            0x58 => { // MOV E,B
                self.e = self.b;
                5
            },
            0x59 => { // MOV E,C
                self.e = self.c;
                5
            },
            0x5a => { // MOV E,D
                self.e = self.d;
                5
            },
            0x5b => { // MOV E,E
                5
            },
            0x5c => { // MOV E,H
                self.e = self.h;
                5
            },
            0x5d => { // MOV E,L
                self.e = self.l;
                5
            },
            0x5e => { // MOV E,M
                self.e = self.read_from_hl();
                7
            },
            0x5f => { // MOV E,A
                self.e = self.a;
                5
            },
            0x60 => { // MOV H,B
                self.h = self.b;
                5
            },
            0x61 => { // MOV H,C
                self.h = self.c;
                5
            },
            0x62 => { // MOV H,D
                self.h = self.d;
                5
            },
            0x63 => { // MOV H,E
                self.h = self.e;
                5
            },
            0x64 => { // MOV H,H
                5
            },
            0x65 => { // MOV H,L
                self.h = self.l;
                5
            },
            0x66 => { // MOV H,M
                self.h = self.read_from_hl();
                7
            },
            0x67 => { // MOV H,A
                self.h = self.a;
                5
            },
            0x68 => { // MOV L,B
                self.l = self.b;
                5
            },
            0x69 => { // MOV L,C
                self.l = self.c;
                5
            },
            0x6a => { // MOV L,D
                self.l = self.d;
                5
            },
            0x6b => { // MOV L,E
                self.l = self.e;
                5
            },
            0x6c => { // MOV L,H
                self.l = self.h;
                5
            },
            0x6d => { // MOV L,L
                5
            },
            0x6e => { // MOV L,M
                self.l = self.read_from_hl();
                7
            },
            0x6f => { // MOV L,A
                self.l = self.a;
                5
            },
            0x70 => { // MOV M,B
                let addr = self.hl();
                self.mem.write(addr, self.b);
                7
            },
            0x71 => { // MOV M,C
                let addr = self.hl();
                self.mem.write(addr, self.c);
                7
            },
            0x72 => { // MOV M,D
                let addr = self.hl();
                self.mem.write(addr, self.d);
                7
            },
            0x73 => { // MOV M,E
                let addr = self.hl();
                self.mem.write(addr, self.e);
                7
            },
            0x74 => { // MOV M,H
                let addr = self.hl();
                self.mem.write(addr, self.h);
                7
            },
            0x75 => { // MOV M,L
                let addr = self.hl();
                self.mem.write(addr, self.l);
                7
            },
            // 0x76 => { // HLT

            // },
            0x77 => { // MOV M,A
                let addr = self.hl();
                self.mem.write(addr, self.a);
                7
            },
            0x78 => { // MOV A,B
                self.a = self.b;
                5
            },
            0x79 => { // MOV A,C
                self.a = self.c;
                5
            },
            0x7a => { // MOV A,D
                self.a = self.d;
                5
            },
            0x7b => { // MOV A,E
                self.a = self.e;
                5
            },
            0x7c => { // MOV A,H
                self.a = self.h;
                5
            },
            0x7d => { // MOV A,L
                self.a = self.l;
                5
            },
            0x7e => { // MOV A,M
                self.a = self.read_from_hl();
                7
            },
            0x7f => { // MOV A,A
                5
            },
            0x80 => { // ADD B
                let x = self.b;
                self.add(x);
                4
            },
            0x81 => { // ADD C
                let x = self.c;
                self.add(x);
                4
            }
            0x82 => { // ADD D
                let x = self.d;
                self.add(x);
                4
            },
            0x83 => { // ADD E
                let x = self.e;
                self.add(x);
                4
            },
            0x84 => { // ADD H
                let x = self.h;
                self.add(x);
                4
            },
            0x85 => { // ADD L
                let x = self.l;
                self.add(x);
                4
            },
            0x86 => { // ADD M
                let x = self.read_from_hl();
                self.add(x);
                7
            },
            0x87 => { // ADD A
                let x = self.a;
                self.add(x);
                4
            },
            0x88 => { // ADC B
                let x = self.b;
                self.adc(x);
                4
            },
            0x89 => { // ADC C
                let x = self.c;
                self.adc(x);
                4
            },
            0x8a => { // ADC D
                let x = self.d;
                self.adc(x);
                4
            },
            0x8b => { // ADC E
                let x = self.e;
                self.adc(x);
                4
            },
            0x8c => { // ADC H
                let x = self.h;
                self.adc(x);
                4
            },
            0x8d => { // ADC L
                let x = self.l;
                self.adc(x);
                4
            },
            0x8e => { // ADC M
                let x = self.read_from_hl();
                self.adc(x);
                7
            },
            0x8f => { // ADC A
                let x = self.a;
                self.adc(x);
                4
            },
            0x90 => { // SUB B
                let x = self.b;
                self.sub(x);
                4
            },
            0x91 => { // SUB C
                let x = self.c;
                self.sub(x);
                4
            },
            0x92 => { // SUB D
                let x = self.d;
                self.sub(x);
                4
            },
            0x93 => { // SUB E
                let x = self.e;
                self.sub(x);
                4
            },
            0x94 => { // SUB H
                let x = self.h;
                self.sub(x);
                4
            },
            0x95 => { // SUB L
                let x = self.l;
                self.sub(x);
                4
            },
            0x96 => { // SUB M
                let x = self.read_from_hl();
                self.sub(x);
                7
            },
            0x97 => { // SUB A
                // TODO: could just set to 0
                let x = self.a;
                self.sub(x);
                4
            },
            0x98 => { // SBB B
                let x = self.b;
                self.sbb(x);
                4
            },
            0x99 => { // SBB C
                let x = self.c;
                self.sbb(x);
                4
            },
            0x9a => { // SBB D
                let x = self.d;
                self.sbb(x);
                4
            },
            0x9b => { // SBB E
                let x = self.e;
                self.sbb(x);
                4
            },
            0x9c => { // SBB H
                let x = self.h;
                self.sbb(x);
                4
            },
            0x9d => { // SBB L
                let x = self.l;
                self.sbb(x);
                4
            },
            0x9e => { // SBB M
                let x = self.read_from_hl();
                self.sbb(x);
                7
            },
            0x9f => { // SBB A
                let x = self.a;
                self.sbb(x);
                4
            },
            0xa0 => { // ANA B
                let x = self.b;
                self.ana(x);
                4
            },
            0xa1 => { // ANA C
                let x = self.c;
                self.ana(x);
                4
            },
            0xa2 => { // ANA D
                let x = self.d;
                self.ana(x);
                4
            },
            0xa3 => { // ANA E
                let x = self.e;
                self.ana(x);
                4
            },
            0xa4 => { // ANA H
                let x = self.h;
                self.ana(x);
                4
            },
            0xa5 => { // ANA L
                let x = self.l;
                self.ana(x);
                4
            },
            0xa6 => { // ANA M
                let x = self.read_from_hl();
                self.ana(x);
                7
            },
            0xa7 => { // ANA A
                let x = self.a;
                self.ana(x);
                4
            },
            0xa8 => { // XRA B
                let x = self.b;
                self.xra(x);
                4
            },
            0xa9 => { // XRA C
                let x = self.c;
                self.xra(x);
                4
            },
            0xaa => { // XRA D
                let x = self.d;
                self.xra(x);
                4
            },
            0xab => { // XRA E
                let x = self.e;
                self.xra(x);
                4
            },
            0xac => { // XRA H
                let x = self.h;
                self.xra(x);
                4
            },
            0xad => { // XRA L
                let x = self.l;
                self.xra(x);
                4
            },
            0xae => { // XRA M
                let x = self.read_from_hl();
                self.xra(x);
                7
            },
            0xaf => { // XRA A
                let x = self.a;
                self.xra(x);
                4
            },
            0xb0 => { // ORA B
                let x = self.b;
                self.ora(x);
                4
            },
            0xb1 => { // ORA C
                let x = self.c;
                self.ora(x);
                4
            },
            0xb2 => { // ORA D
                let x = self.d;
                self.ora(x);
                4
            },
            0xb3 => { // ORA E
                let x = self.e;
                self.ora(x);
                4
            },
            0xb4 => { // ORA H
                let x = self.h;
                self.ora(x);
                4
            },
            0xb5 => { // ORA L
                let x = self.l;
                self.ora(x);
                4
            },
            0xb6 => { // ORA M
                let x = self.read_from_hl();
                self.ora(x);
                7
            },
            0xb7 => { // ORA A
                let x = self.a;
                self.ora(x);
                4
            },
            0xb8 => { // CMP B
                let x = self.b;
                self.cmp(x);
                4
            },
            0xb9 => { // CMP C
                let x = self.c;
                self.cmp(x);
                4
            },
            0xba => { // CMP D
                let x = self.d;
                self.cmp(x);
                4
            },
            0xbb => { // CMP E
                let x = self.e;
                self.cmp(x);
                4
            },
            0xbc => { // CMP H
                let x = self.h;
                self.cmp(x);
                4
            },
            0xbd => { // CMP L
                let x = self.l;
                self.cmp(x);
                4
            },
            0xbe => { // CMP M
                let x = self.read_from_hl();
                self.cmp(x);
                7
            },
            0xbf => { // CMP A
                let x = self.a;
                self.cmp(x);
                4
            },
            0xc0 => { // RNZ
                if self.cc.z == 0 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xc1 => { // POP B
                let (lo, hi) = self.pop();
                self.c = lo;
                self.b = hi;
                10
            },
            0xc2 => { // JNZ
                if self.cc.z == 0 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                    10
                }
            },
            0xc3 => { // JMP addr
                self.pc = self.read_u16();
                // Return here so we don't increment
                // the program counter at the end of the method.
                return 10;
            },
            0xc4 => { // CNZ addr
                if self.cc.z == 0 {
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
            },
            0xc5 => { // PUSH B
                let (lo, hi) = (self.c, self.b);
                self.push(lo, hi);
                11
            },
            0xc6 => { // ADI D8
                let x = self.read_byte();
                let a = (self.a as u16).wrapping_add(x as u16);
                self.set_flags_arith(a);
                self.a = a as u8;
                7
            },
            0xc8 => { // RZ
                if self.cc.z == 1 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xc9 => { // RET
                self.ret();
                return 10;
            },
            0xca => { // JZ addr
                if self.cc.z == 1 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xcc => { // CZ addr
                if self.cc.z == 1 {
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
            },
            0xcd => { // CALL addr
                self.call();
                return 17;
            },
            0xce => { // ACI D8
                let x = self.read_byte();
                let a = (self.a as u16)
                    .wrapping_add(x as u16)
                    .wrapping_add(self.cc.cy as u16);
                self.set_flags_arith(a);
                self.a = a as u8;
                7
            },
            0xd0 => { // RNC
                if self.cc.cy == 0 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xd1 => { // POP D
                let (lo, hi) = self.pop();
                self.e = lo;
                self.d = hi;
                10
            },
            0xd2 => { // JNC addr
                if self.cc.cy == 0 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xd3 => { // OUT D8
                let port = self.read_byte();
                self.ports[port as usize] = self.a;
                10
            },
            0xd4 => { // CNC addr
                if self.cc.cy == 0 {
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
            },
            0xd5 => { // PUSH D
                let (lo, hi) = (self.e, self.d);
                self.push(lo, hi);
                11
            },
            0xd6 => { // SUI D8
                let x = self.read_byte();
                let a = (self.a as u16).wrapping_sub(x as u16);
                self.set_flags_arith(a);
                self.a = a as u8;
                7
            },
            0xd8 => { // RC
                if self.cc.cy == 1 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xda => { // JC addr
                if self.cc.cy == 1 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xdb => { // IN port
                let port = self.read_byte();
                self.a = self.ports[port as usize];
                10
            },
            0xdc => { // CC addr
                if self.cc.cy == 1 {
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
            },
            0xde => { // SBI D8
                let x = self.read_byte();
                let a = self.a as u16 - x as u16 - self.cc.cy as u16;
                self.set_flags_arith(a);
                self.a = a as u8;
                7
            },
            0xe0 => { // RPO
                if self.cc.p == 0 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xe1 => { // POP H
                let (lo, hi) = self.pop();
                self.l = lo;
                self.h = hi;
                10
            },
            0xe2 => { // JPO addr
                if self.cc.p == 0 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xe3 => { // XTHL
                let l = self.l;
                let h = self.h;
                self.l = self.mem.read(self.sp);
                self.h = self.mem.read(self.sp + 1);
                self.mem.write(self.sp, l);
                self.mem.write(self.sp + 1, h);
                18
            },
            0xe4 => { // CPO addr
                if self.cc.p == 0 {
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
            },
            0xe5 => { // PUSH H
                let (lo, hi) = (self.l, self.h);
                self.push(lo, hi);
                11
            },
            0xe6 => { // ANI D8
                let x = self.read_byte();
                let a = self.a & x;
                self.set_flags_logic(a);
                self.a = a;
                7
            },
            0xe8 => { // RPE
                if self.cc.p == 1 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xe9 => { // PCHL
                self.pc = self.hl();
                return 5;
            },
            0xea => { // JPE addr
                if self.cc.p == 1 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xeb => { // XCHG
                mem::swap(&mut self.e, &mut self.l);
                mem::swap(&mut self.d, &mut self.h);
                4
            },
            0xec => { // CPE addr
                if self.cc.p == 1 {
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
            },
            0xee => { // XRI D8
                let x = self.read_byte();
                let a = self.a ^ x;
                self.set_flags_logic(a);
                self.a = a;
                7
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
                10
            },
            0xf0 => { // RP
                if self.cc.s == 0 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xf2 => { // JP addr
                if self.cc.s == 0 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xf4 => { // CP addr
                if self.cc.s == 0 {
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
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
                11
            },
            0xf6 => { // ORI D8
                let a = self.a | self.read_byte();
                self.set_flags_logic(a);
                self.a = a;
                7
            },
            0xf8 => { // RM
                if self.cc.s == 1 {
                    self.ret();
                    return 11;
                } else {
                    5
                }
            },
            0xfa => { // JM addr
                if self.cc.s == 1 {
                    self.pc = self.read_u16();
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xfb => { // EI
                self.interrupt_enabled = true;
                4
            },
            0xfc => { // CM addr
                if self.cc.s == 1{
                    self.call();
                    return 17;
                } else {
                    self.pc += 2;
                }
                11
            },
            0xfe => { // CPI D8
                let x = self.read_byte();
                self.cmp(x);
                7
            },
            x => panic!("Unimplemented OpCode: {:>0pad$x}", x, pad=2)
        };
        self.pc += 1;
        cycles
    }

    // TODO: safe interface for reading/writing from/to memory.
    // This will allow the enforcement of a maximum memory size.
}

#[test]
fn test_parity_even1() {
    let p = parity(0b11001100);
    assert_eq!(p, 1);
}

#[test]
fn test_parity_even2() {
    let p = parity(0b11010001);
    assert_eq!(p, 1);
}

#[test]
fn test_parity_odd1() {
    let p = parity(0b1110000);
    assert_eq!(p, 0);
}

#[test]
fn test_parity_odd2() {
    let p = parity(0b1000000);
    assert_eq!(p, 0);
}
