
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RegPair {
    BC,
    DE,
    HL,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Reg {
    A, B, C, D, E, H, L
}

/// A trait representing the abstract function of
/// a machine running an 8080 processor.
pub trait Machine {
    /// Step the Machine by a single instruction.
    fn step(&mut self) -> u32;

    /// Generate an interrupt to the Machine.
    fn interrupt(&mut self, code: usize);

    fn get_pc(&self) -> u16;

    fn get_a(&self) -> u8;
    fn set_a(&mut self, value: u8);

    fn get_b(&self) -> u8;
    fn set_b(&mut self, value: u8);

    fn get_c(&self) -> u8;
    fn set_c(&mut self, value: u8);

    fn get_d(&self) -> u8;
    fn set_d(&mut self, value: u8);

    fn get_e(&self) -> u8;
    fn set_e(&mut self, value: u8);

    fn get_h(&self) -> u8;
    fn set_h(&mut self, value: u8);

    fn get_l(&self) -> u8;
    fn set_l(&mut self, value: u8);

    /// Read a byte from memory.
    fn read(&self, addr: u16) -> u8;
    /// Write a byte to memory.
    fn write(&mut self, addr: u16, value: u8);
}