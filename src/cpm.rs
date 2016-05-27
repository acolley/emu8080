
use cpu::{Cpu, make_u16};
use machine::Machine;
use memory::Memory;

/// Emulation of the CP/M system.
pub struct Cpm {
    pub cpu: Cpu,
}

impl Cpm {
    pub fn new(data: &[u8]) -> Self {
        // In a CP/M system we offset
        // data in memory by 0x100.
        let memory = Memory::with_data_and_offset(data, 0x100);
        Cpm {
            cpu: Cpu::new(memory),
        }
    }

    pub fn run(&mut self) {
        loop {
            self.step();
        }
    }
}

impl Machine for Cpm {
    /// Step the Machine by a single instruction.
    #[inline(always)]
    fn step(&mut self) -> u32 {
        let op = self.cpu.mem.read(self.cpu.pc);
        // println!("pc: {:>0pad$x} op: {:>0padop$x} cc.p: {}", self.cpu.pc, op, self.cpu.cc.p, pad=4, padop=2);
        let cycles = match op {
            0xcd => {
                let addr = self.cpu.read_u16();
                match addr {
                    5 => {
                        if self.cpu.c == 9 {
                            let offset = make_u16(self.cpu.e, self.cpu.d) + 3;
                            let mut c = String::from_utf8(vec![self.cpu.mem.read(offset)]).expect("Not a UTF-8 String");
                            loop {
                                match c.as_ref() {
                                    "$" => break,
                                    c => print!("{}", c),
                                }
                            }
                            print!("\n");
                        }
                        17
                    },
                    0 => {
                        ::std::process::exit(0);
                        17
                    },
                    _ => self.cpu.step()
                }
            },
            _ => self.cpu.step()
        };
        cycles
    }

    #[inline(always)]
    fn interrupt(&mut self, int: usize) {
        // TODO: interrupt code should be an enum?
        // PUSH PC
        let hi = ((self.cpu.pc & 0xff00) >> 8) as u8;
        let lo = (self.cpu.pc & 0xff) as u8;
        self.cpu.push(hi, lo);
        // Set PC to low memory vector
        // This is identical to an `RST int` instruction
        self.cpu.pc = (8 * int) as u16;
        self.cpu.interrupt_enabled = false;
    }

    #[inline(always)]
    fn get_pc(&self) -> u16 { self.cpu.pc }

    #[inline(always)]
    fn get_a(&self) -> u8 { self.cpu.a }
    #[inline(always)]
    fn set_a(&mut self, value: u8) { self.cpu.a = value; }

    #[inline(always)]
    fn get_b(&self) -> u8 { self.cpu.b }
    #[inline(always)]
    fn set_b(&mut self, value: u8) { self.cpu.b = value; }

    #[inline(always)]
    fn get_c(&self) -> u8 { self.cpu.c }
    #[inline(always)]
    fn set_c(&mut self, value: u8) { self.cpu.c = value; }

    #[inline(always)]
    fn get_d(&self) -> u8 { self.cpu.d }
    #[inline(always)]
    fn set_d(&mut self, value: u8) { self.cpu.d = value; }

    #[inline(always)]
    fn get_e(&self) -> u8 { self.cpu.e }
    #[inline(always)]
    fn set_e(&mut self, value: u8) { self.cpu.e = value; }

    #[inline(always)]
    fn get_h(&self) -> u8 { self.cpu.h }
    #[inline(always)]
    fn set_h(&mut self, value: u8) { self.cpu.h = value; }

    #[inline(always)]
    fn get_l(&self) -> u8 { self.cpu.l }
    #[inline(always)]
    fn set_l(&mut self, value: u8) { self.cpu.l = value; }

    #[inline(always)]
    fn read(&self, addr: u16) -> u8 {
        self.cpu.mem.read(addr)
    }

    #[inline(always)]
    fn write(&mut self, addr: u16, value: u8) {
        self.cpu.mem.write(addr, value);
    }
}
