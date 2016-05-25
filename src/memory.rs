
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
            panic!("Trying to write {:>0padd$x} to ROM at: {:>0pada$x}", d, addr, padd=2, pada=4);
        }
        let Memory(ref mut mem) = *self;
        mem[addr as usize] = d;
    }
}