
mod bus;
mod olc6502;
mod olc2C02;
mod cartridge;

use bus::Bus;
use olc6502::CPU;

fn main() {
    let mut cpu = CPU::new();
    
}
