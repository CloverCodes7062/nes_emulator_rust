use crate::olc6502::CPU;

#[derive(Debug)]
pub struct Bus {
    cpu_ram: [u8; 2048],
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            cpu_ram: [0; 2048],
        }
    }

    pub fn cpu_write(&mut self, addr: u16, data: u8) {
        if addr >= 0x0000 && addr <= 0x1FFF {
            self.cpu_ram[(addr & 0x07FF) as usize] = data;
        }
    }

    pub fn cpu_read(&mut self, addr: u16) -> u8 {
        let mut data: u8 = 0x00;

        if addr >= 0x0000 && addr <= 0x1FFF {
            data = self.cpu_ram[(addr & 0x07FF) as usize];
        }

        data
    }

    pub fn cpu_read_with_read_only(&mut self, addr: u16, b_read_only: bool) -> u8 {
        let mut data: u8 = 0x00;

        if addr >= 0x0000 && addr <= 0x1FFF {
            data = self.cpu_ram[(addr & 0x07FF) as usize];
        }

        data
    }
}