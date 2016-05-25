
use std::mem;

use memory::Memory;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConditionCodes {
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
}

#[inline(always)]
fn parity(x: u8, size: u8) -> u8 {
    let mut p = 0;
    let mut x = x & ((1u8.wrapping_shl(size as u32)) - 1);
    for _ in 0..size {
        if (x & 0x1) > 0 {
            p += 1;
        }
        x = x.wrapping_shr(1);
    }

    if (p & 0x1) == 0 { 1 } else { 0 }
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
fn make_u16(lo: u8, hi: u8) -> u16 {
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
    fn read_u16(&mut self) -> u16 {
        let lo = self.mem.read(self.pc + 1);
        let hi = self.mem.read(self.pc + 2);
        self.pc += 2;
        make_u16(lo, hi)
    }

    /// Read from the memory address given
    /// by the bytes held in the H & L registers.
    #[inline(always)]
    fn read_from_hl(&self) -> u8 {
        let addr = make_u16(self.l, self.h);
        self.mem.read(addr)
    }

    #[inline(always)]
    fn set_flags_zsp(&mut self, x: u8) {
        self.cc.z = if x == 0 { 1 } else { 0 };
        self.cc.s = if (x & 0x80 == 0x80) { 1 } else { 0 };
        self.cc.p = parity(x, 8);
    }

    #[inline(always)]
    fn set_flags_arith(&mut self, x: u16) {
        self.set_flags_zsp(x as u8);
        self.cc.cy = if x > 0xff { 1 } else { 0 };
    }

    #[inline(always)]
    fn set_flags_logic(&mut self, x: u8) {
        self.set_flags_zsp(x);
        self.cc.cy = 0;
        self.cc.ac = 0;
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
                let addr = make_u16(self.b, self.c);
                self.a = self.mem.read(addr);
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
            0x09 => { // DAD B
                let bc = (self.b as u32) << 8 | (self.c as u32);
                let hl = (self.h as u32) << 8 | (self.l as u32);
                let hl = hl.wrapping_add(bc);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) > 0 { 1 } else { 0 };
                10
            },
            0x0a => { // LDAX B
                let addr = (self.b as u16) << 8 | (self.c as u16);
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
                // If register had a bit switched on in the LSB
                // then rotating right will have caused a carry.
                if self.a & 0x01 == 1 {
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
            0x19 => { // DAD D
                let de = (self.d as u32) << 8 | (self.e as u32);
                let hl = (self.h as u32) << 8 | (self.l as u32);
                let hl = hl.wrapping_add(de);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) != 0 { 1 } else { 0 };
                10
            },
            0x1a => { // LDAX D
                let addr = (self.d as u16) << 8 | (self.e as u16);
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
            0x26 => { // MVI H,D8
                self.h = self.read_byte();
                7
            },
            0x27 => { // DAA
                // The eight-bit number in the accumulator is adjusted 
                // to form two four-bit Binary-Coded-Decimal digits 
                // by the following Process:
                // 1. If the value of the least significant 4 bits of the 
                // accumulator is greater than 9 OR if the AC flag is set, 
                // 6 is added to the accumulator.
                // 2. If the value of the most significant 4 bits of the 
                // accumulator is now greater than 9, or if the CY flag is set, 
                // 6 is added to the most significant 4 bits of the accumulator.
                if self.a & 0xf > 9 {
                    self.a += 9;
                }
                if self.a & 0xf0 > 0x90 {
                    let res = (self.a as u16) + 0x60;
                    self.a = res as u8;
                    self.set_flags_arith(res);
                }
                4
            },
            0x29 => { // DAD H
                let hl = (self.h as u32) << 8 | (self.l as u32);
                let hl = hl.wrapping_add(hl);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                self.cc.cy = if (hl & 0xffff0000) != 0 { 1 } else { 0 };
                10
            },
            0x2e => { // MVI L,D8
                self.l = self.read_byte();
                7
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
            0x35 => { // DCR M
                let hl = (self.h as u16) << 8 | (self.l as u16);
                let x = self.mem.read(hl).wrapping_sub(1);
                self.set_flags_zsp(x);
                self.mem.write(hl, x);
                10
            },
            0x36 => { // MVI M,D8
                let x = self.read_byte();
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.mem.write(addr, x);
                10
            },
            0x37 => { // STC
                self.cc.cy = 1;
                4
            },
            0x39 => { // DAD SP
                let hl = (self.h as u32) << 8 | (self.l as u32);
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
            0x45 => { // MOV B,L
                self.b = self.l;
                5
            },
            0x46 => { // MOV B,M
                self.b = self.read_from_hl();
                7
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
            0x4d => { // MOV C,L
                self.c = self.l;
                5
            },
            0x4c => { // MOV C,H
                self.c = self.h;
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
            0x51 => { // MOV D,C
                self.d = self.c;
                5
            },
            0x56 => { // MOV D,M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.d = self.mem.read(addr);
                7
            },
            0x57 => { // MOV D,A
                self.d = self.a;
                5
            },
            0x5e => { // MOV E,M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.e = self.mem.read(addr);
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
            0x66 => { // MOV H,M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.h = self.mem.read(addr);
                7
            },
            0x67 => { // MOV H,A
                self.h = self.a;
                5
            },
            0x6f => { // MOV L,A
                self.l = self.a;
                5
            },
            0x77 => { // MOV M,A
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.mem.write(addr, self.a);
                7
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
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.a = self.mem.read(addr);
                7
            },
            0x7f => { // MOV A,A
                5
            },
            // 0x80 => { // ADD B
            //     let result = self.a as u16 + self.b as u16;
            //     self.cc.update(result);
            //     // store result in register A
            //     self.a = (result & 0xff) as u8;
            // },
            0x82 => { // ADD D
                let x = (self.a as u16) + (self.d as u16);
                self.set_flags_arith(x);
                self.a = x as u8;
                4
            },
            0x87 => { // ADD A
                let x = (self.a as u16) + (self.a as u16);
                self.set_flags_arith(x);
                self.a = x as u8;
                4
            },
            0xa7 => { // ANA A
                let a = self.a & self.a;
                self.set_flags_logic(a);
                self.a = a;
                4
            },
            0xaf => { // XRA A
                let a = self.a ^ self.a;
                self.set_flags_logic(a);
                self.a = a;
                4
            },
            0xb0 => { // ORA B
                let a = self.a | self.b;
                self.set_flags_logic(a);
                self.a = a;
                4
            },
            0xb3 => { // ORA E
                let a = self.a | self.e;
                self.set_flags_logic(a);
                self.a = a;
                4
            },
            0xb6 => { // ORA M
                let x = self.read_from_hl();
                let a = self.a | x;
                self.set_flags_logic(a);
                self.a = a;
                7
            },
            0xbe => { // CMP M
                let x = self.read_from_hl();
                let res = self.a as u16 - x as u16;
                self.set_flags_arith(res);
                7
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
            0xcd => { // CALL addr
                // Push return address on to stack
                let ret = self.pc + 3;
                let (lo, hi) = split_u16(ret);
                self.push(lo, hi);
                self.pc = self.read_u16();
                return 17;
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
            0xe1 => { // POP H
                let (lo, hi) = self.pop();
                self.l = lo;
                self.h = hi;
                10
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
            0xeb => { // XCHG
                mem::swap(&mut self.e, &mut self.l);
                mem::swap(&mut self.d, &mut self.h);
                4
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
            0xfe => { // CPI D8
                let x = self.read_byte();
                let y = self.a.wrapping_sub(x);
                self.set_flags_zsp(y);
                self.cc.cy = if self.a < x { 1 } else { 0 };
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
fn test_nop() {
    let mut cpu = Cpu::with_data(&[0x00]);
    cpu.step();
    let mut expected = Cpu::new();
    expected.pc += 1;
    assert_eq!(cpu, expected);
}

#[test]
fn test_lxi_b() {
    let mut cpu = Cpu::with_data(&[0x01, 0xfe, 0xff]);
    let mut expected = cpu.clone();
    expected.pc = 3;
    expected.b = 0xff;
    expected.c = 0xfe;

    cpu.step();
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

    cpu.step();
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

    cpu.step();
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

    cpu.step();
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

    cpu.step();
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

    cpu.step();
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

    cpu.step();
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

    cpu.step();
    assert_eq!(cpu, expected);
}