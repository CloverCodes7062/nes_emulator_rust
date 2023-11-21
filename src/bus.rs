use crate::olc6502::CPU;

pub struct Bus {
    ram: [u8; 64 * 1024],
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            ram: [0; 64 * 1024],
        }
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        if addr >= 0x0000 && addr <= 0xFFFF {
            self.ram[addr as usize] = data;
        }
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        if addr >= 0x0000 && addr <= 0xFFFF {
            self.ram[addr as usize]
        } else {
            0x00
        }
    }

    pub fn read_with_read_only(&mut self, addr: u16, b_read_only: bool) -> u8 {
        if addr >= 0x0000 && addr <= 0xFFFF {
            self.ram[addr as usize]
        } else {
            0x00
        }
    }
}