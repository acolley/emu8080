
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Memory(Vec<u8>);

impl Memory {
    pub fn with_data(data: &[u8]) -> Self {
        Self::with_data_and_offset(data, 0)
    }

    pub fn with_data_and_offset(data: &[u8], offset: usize) -> Self {
        let mut mem = Vec::with_capacity(65536);
        mem.resize(65536, 0);
        for (i, x) in data.iter().enumerate() {
            mem[i + offset] = x.clone();
        }
        Memory(mem)
    }

    #[inline(always)]
    pub fn read(&self, addr: u16) -> u8 {
        let Memory(ref mem) = *self;
        mem[addr as usize]
    }

    #[inline(always)]
    pub fn write(&mut self, addr: u16, d: u8) {
        if addr < 0x2000 {
            panic!("Trying to write to ROM at: {:>0pad$x}", addr, pad=4)
        }
        let Memory(ref mut mem) = *self;
        mem[addr as usize] = d;
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

    if (p & 0x1) == 0 {
        1
    } else {
        0
    }
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
    pub fn with_data(data: &[u8]) -> Self {
        Self::with_data_and_offset(data, 0)
    }

    pub fn with_data_and_offset(data: &[u8], offset: usize) -> Self {
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
            mem: Memory::with_data_and_offset(data, offset),
            cc: ConditionCodes::new(),
            interrupt_enabled: false,
            ports: [0; 8],
            speed: 2_000_000,
        }
    }

    #[inline(always)]
    pub fn push(&mut self, hi: u8, lo: u8) {
        self.mem.write(self.sp - 1, hi);
        self.mem.write(self.sp - 2, lo);
        self.sp -= 2;
    }

    /// Pop (hi, lo) from stack.
    #[inline(always)]
    pub fn pop(&mut self) -> (u8, u8) {
        self.sp += 2;
        (self.mem.read(self.sp - 1), self.mem.read(self.sp - 2))
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
                self.c = self.mem.read(self.pc + 1);
                self.b = self.mem.read(self.pc + 2);
                self.pc += 2;
                10
            },
            0x02 => { // STAX B
                let addr = (self.c as u16) << 8 | (self.b as u16);
                self.a = self.mem.read(addr);
                7
            },
            0x03 => { // INX B
                let bc = (self.c as u16) << 8 | (self.b as u16);
                let bc = bc.wrapping_add(1);
                self.b = (bc & 0xff) as u8;
                self.c = ((bc & 0xff00) >> 8) as u8;
                5
            },
            0x04 => { // INR B
                let b = self.b.wrapping_add(1);
                self.cc.z = if b == 0 { 1 } else { 0 };
                self.cc.s = if (b & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(b, 8);
                self.b = b;
                5
            },
            0x05 => { // DCR B
                let b = self.b.wrapping_sub(1);
                self.cc.z = if b == 0 { 1 } else { 0 };
                self.cc.s = if (b & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(b, 8);
                self.b = b;
                5
            },
            0x06 => { // MVI B,D8
                let x = self.mem.read(self.pc + 1);
                self.b = x;
                self.pc += 1;
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
                let bc = (self.b as u16) << 8 | (self.c as u16);
                let bc = bc.wrapping_sub(1);
                self.e = (bc & 0xff) as u8;
                self.d = ((bc & 0xff00) >> 8) as u8;
                5
            },
            0x0d => { // DCR C
                let c = self.c.wrapping_sub(1);
                self.cc.z = if c == 0 { 1 } else { 0 };
                self.cc.s = if (c & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(c, 8);
                self.c = c;
                5
            },
            0x0e => { // MVI C,D8
                let x = self.mem.read(self.pc + 1);
                self.c = x;
                self.pc += 1;
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
                let d = self.mem.read(self.pc + 2);
                let e = self.mem.read(self.pc + 1);
                self.d = d;
                self.e = e;
                self.pc += 2;
                10
            },
            0x13 => { // INX D
                let de = (self.d as u16) << 8 | (self.e as u16);
                let de = de.wrapping_add(1);
                self.e = (de & 0xff) as u8;
                self.d = ((de & 0xff00) >> 8) as u8;
                5
            },
            0x14 => { // INR D
                self.d = self.d.wrapping_add(1);
                self.cc.z = if self.d == 0 { 1 } else { 0 };
                self.cc.s = if (self.d & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(self.d, 8);
                5
            },
            0x18 => { 0 }, // Nothing?
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
                let de = (self.d as u16) << 8 | (self.e as u16);
                let de = de.wrapping_sub(1);
                self.e = (de & 0xff) as u8;
                self.d = ((de & 0xff00) >> 8) as u8;
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
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.l = lo;
                self.h = hi;
                self.pc += 2;
                10
            },
            0x22 => { // SHLD addr
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                let addr = (hi as u16) << 8 | (lo as u16);
                self.mem.write(addr, self.l);
                self.mem.write(addr + 1, self.h);
                self.pc += 2;
                16
            },
            0x23 => { // INX H
                let hl = (self.h as u16) << 8 | (self.l as u16);
                let hl = hl.wrapping_add(1);
                self.l = (hl & 0xff) as u8;
                self.h = ((hl & 0xff00) >> 8) as u8;
                5
            },
            0x24 => { // INR H
                let h = self.h.wrapping_add(1);
                self.cc.z = if h == 0 { 1 } else { 0 };
                self.cc.s = if (h & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(h, 8);
                self.h = h;
                5
            },
            0x26 => { // MVI H,D8
                let x = self.mem.read(self.pc + 1);
                self.h = x;
                self.pc += 1;
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
                    self.cc.cy = if res > 0xff { 1 } else { 0 };
                    self.cc.z = if (res & 0xff) == 0 { 1 } else { 0 };
                    self.cc.s = if (res & 0x80) == 0x80 { 1 } else { 0 };
                    self.cc.p = parity(res as u8, 8);
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
            0x31 => { // LXI SP,D16
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.sp = (hi as u16) << 8 | (lo as u16);
                self.pc += 2;
                10
            },
            0x32 => { // STA addr
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                let addr = (hi as u16) << 8 | (lo as u16);
                self.mem.write(addr, self.a);
                self.pc += 2;
                13
            },
            0x35 => { // DCR M
                let hl = (self.h as u16) << 8 | (self.l as u16);
                let x = self.mem.read(hl).wrapping_sub(1);
                self.cc.z = if x == 0 { 1 } else { 0 };
                self.cc.s = if (x & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(x, 8);
                self.mem.write(hl, x);
                10
            },
            0x36 => { // MVI M,D8
                let x = self.mem.read(self.pc + 1);
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.mem.write(addr, x);
                self.pc += 1;
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
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                let addr = (hi as u16) << 8 | (lo as u16);
                self.a = self.mem.read(addr);
                self.pc += 2;
                13
            },
            0x3d => { // DCR A
                let a = self.a.wrapping_sub(1);
                self.cc.z = if a == 0 { 1 } else { 0 };
                self.cc.s = if (a & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(a, 8);
                self.a = a;
                5
            },
            0x3e => { // MVI A,D8
                let x = self.mem.read(self.pc + 1);
                self.a = x;
                self.pc += 1;
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
                let addr = (self.h as u16) << 8 | (self.l as u16);
                self.c = self.mem.read(addr);
                7
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
                self.cc.z = if x & 0xff == 0 { 1 } else { 0 };
                self.cc.s = if x & 0x80 > 0 { 1 } else { 0 };
                self.cc.cy = if x > 0xff { 1 } else { 0 };
                self.cc.p = parity((x & 0xff) as u8, 8);
                self.a = x as u8;
                self.pc += 1;
                4
            },
            0x87 => { // ADD A
                let x = (self.a as u16) + (self.a as u16);
                self.cc.z = if x & 0xff == 0 { 1 } else { 0 };
                self.cc.s = if x & 0x80 > 0 { 1 } else { 0 };
                self.cc.cy = if x > 0xff { 1 } else { 0 };
                self.cc.p = parity((x & 0xff) as u8, 8);
                self.a = x as u8;
                self.pc += 1;
                4
            },
            0xa7 => { // ANA A
                self.a = self.a & self.a;
                self.cc.cy = 0;
                self.cc.ac = 0;
                self.cc.z = if self.a == 0 { 1 } else { 0 };
                self.cc.s = if (self.a & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(self.a, 8);
                4
            },
            0xaf => { // XRA A
                self.a = self.a ^ self.a;
                self.cc.cy = 0;
                self.cc.ac = 0;
                self.cc.z = if self.a == 0 { 1 } else { 0 };
                self.cc.s = if (self.a & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(self.a, 8);
                4
            },
            0xb3 => { // ORA E
                self.a = self.a | self.e;
                self.cc.cy = 0;
                self.cc.ac = 0;
                self.cc.z = if self.a == 0 { 1 } else { 0 };
                self.cc.s = if (self.a & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(self.a, 8);
                4
            },
            0xbe => { // CMP M
                let addr = (self.h as u16) << 8 | (self.l as u16);
                let x = self.mem.read(addr);
                let res = self.a - x;
                self.cc.cy = if self.a < x { 1 } else { 0 };
                self.cc.z = if self.a == x { 1 } else { 0 };
                self.cc.s = if (res & 0x80) == 0x80 { 1 } else { 0 };
                self.cc.p = parity(res, 8);
                7
            },
            0xc0 => { // RNZ
                if self.cc.z == 0 {
                    let lo = self.mem.read(self.sp);
                    let hi = self.mem.read(self.sp + 1);
                    self.pc = (hi as u16) << 8 | (lo as u16);
                    self.sp += 2;
                    return 11;
                } else {
                    5
                }
            },
            0xc1 => { // POP B
                self.c = self.mem.read(self.sp);
                self.b = self.mem.read(self.sp + 1);
                self.sp += 2;
                10
            },
            0xc2 => { // JNZ
                if self.cc.z == 0 {
                    let lo = self.mem.read(self.pc + 1);
                    let hi = self.mem.read(self.pc + 2);
                    self.pc = (hi as u16) << 8 | (lo as u16);
                    return 10;
                } else {
                    self.pc += 2;
                    10
                }
            },
            0xc3 => { // JMP addr
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.pc = (hi as u16) << 8 | (lo as u16);
                // Return here so we don't increment
                // the program counter at the end of the method.
                return 10;
            },
            0xc5 => { // PUSH B
                self.mem.write(self.sp - 1, self.b);
                self.mem.write(self.sp - 2, self.c);
                self.sp -= 2;
                11
            },
            0xc6 => { // ADI D8
                let x = (self.a as u16) + self.mem.read(self.pc + 1) as u16;
                // If x is zero, set zero flag to 1, else 0.
                self.cc.z = if x & 0xff == 0 { 1 } else { 0 };
                // Sign flag: if bit 7 is set, set the flag, else clear.
                self.cc.s = if x & 0x80 > 0 { 1 } else { 0 };
                // Carry flag
                self.cc.cy = if x > 0xff { 1 } else { 0 };               
                // Parity flag
                self.cc.p = parity((x & 0xff) as u8, 8);
                self.a = x as u8;
                self.pc += 1;
                7
            },
            0xc8 => { // RZ
                if self.cc.z == 1 {
                    let lo = self.mem.read(self.sp);
                    let hi = self.mem.read(self.sp + 1);
                    self.pc = (hi as u16) << 8 | (lo as u16);
                    self.sp += 2;
                    return 11;
                } else {
                    5
                }
            },
            0xc9 => { // RET
                let lo = self.mem.read(self.sp);
                let hi = self.mem.read(self.sp + 1);
                self.pc = (hi as u16) << 8 | (lo as u16);
                self.sp += 2;
                return 10;
            },
            0xca => { // JZ addr
                if self.cc.z == 1 {
                    let lo = self.mem.read(self.pc + 1);
                    let hi = self.mem.read(self.pc + 2);
                    self.pc = (hi as u16) << 8 | (lo as u16);
                    return 10;
                } else {
                    10
                }
            },
            0xcd => { // CALL addr
                // Save return address
                let ret = self.pc + 3;
                let lo = (ret & 0x00ff) as u8;
                let hi = ((ret & 0xff00) >> 8) as u8;
                self.mem.write(self.sp - 1, hi);
                self.mem.write(self.sp - 2, lo);
                let lo = self.mem.read(self.pc + 1);
                let hi = self.mem.read(self.pc + 2);
                self.sp -= 2;
                self.pc = (hi as u16) << 8 | (lo as u16);
                return 17;
            },
            0xd1 => { // POP D
                self.e = self.mem.read(self.sp);
                self.d = self.mem.read(self.sp + 1);
                self.sp += 2;
                10
            },
            0xd2 => { // JNC addr
                if self.cc.cy == 0 {
                    let lo = self.mem.read(self.pc + 1);
                    let hi = self.mem.read(self.pc + 2);
                    self.pc = (hi as u16) << 8 | (lo as u16);
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xd3 => { // OUT D8
                let port = self.mem.read(self.pc + 1);
                self.ports[port as usize] = self.a;
                self.pc += 1;
                10
            },
            0xd5 => { // PUSH D
                self.mem.write(self.sp - 1, self.d);
                self.mem.write(self.sp - 2, self.e);
                self.sp -= 2;
                11
            },
            0xda => { // JC addr
                if self.cc.cy == 1 {
                    let lo = self.mem.read(self.pc + 1);
                    let hi = self.mem.read(self.pc + 2);
                    self.pc = (hi as u16) << 8 | (lo as u16);
                    return 10;
                } else {
                    self.pc += 2;
                }
                10
            },
            0xdb => { // IN port
                let port = self.mem.read(self.pc + 1);
                self.a = self.ports[port as usize];
                self.pc += 1;
                10
            },
            0xe1 => { // POP H
                self.l = self.mem.read(self.sp);
                self.h = self.mem.read(self.sp + 1);
                self.sp += 2;
                10
            },
            0xe5 => { // PUSH H
                self.mem.write(self.sp - 1, self.h);
                self.mem.write(self.sp - 2, self.l);
                self.sp -= 2;
                11
            },
            0xe6 => { // ANI D8
                let x = self.mem.read(self.pc + 1);
                self.a = self.a & x;
                self.cc.cy = 0;
                self.cc.ac = 0;
                self.cc.z = if self.a == 0 { 1 } else { 0 };
                self.cc.s = if (self.a & 0x80) == 0x80 { 1 } else { 0 };
                self.pc += 1;
                7
            },
            0xeb => { // XCHG
                let d = self.d;
                let e = self.e;
                self.d = self.h;
                self.e = self.l;
                self.h = d;
                self.l = e;
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
            0xfb => { // EI
                self.interrupt_enabled = true;
                4
            },
            0xfe => { // CPI D8
                let x = self.mem.read(self.pc + 1);
                let y = self.a.wrapping_sub(x);
                self.cc.z = if y == 0 { 1 } else { 0 };
                self.cc.s = if y & 0x08 == 0x08 { 1 } else { 0 };
                self.cc.p = parity(x, 8);
                self.cc.cy = if self.a < x { 1 } else { 0 };
                self.pc += 1;
                7
            },
            x => panic!("Unimplemented Op code: {:>0pad$x}", x, pad=2)
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