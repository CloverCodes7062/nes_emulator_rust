use std::vec;

use crate::bus::Bus;


#[derive(Debug)]
pub struct CPU {
    bus: Bus,
    registers: Registers,
    fetched: u8,
    addr_abs: u16,
    addr_rel: u8,
    opcode: u8,
    cycles: u8,
    lookup: [INSTRUCTION; 256],
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            bus: Bus::new(),
            registers: Registers::new(),
            fetched: 0x00,
            addr_abs: 0x0000,
            addr_rel: 0x00,
            opcode: 0x00,
            cycles: 0x00,
            lookup: [
                INSTRUCTION{ name: "BRK".to_string(), operate: CPU::brk, addr_mode: CPU::imm, cycles: 7 }, INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::izx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 3 }, INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "ASL".to_string(), operate: CPU::asl, addr_mode: CPU::zp0, cycles: 5 },
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "PHP".to_string(), operate: CPU::php, addr_mode: CPU::imp, cycles: 3 }, INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "ASL".to_string(), operate: CPU::asl, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::abs, cycles: 4 }, 
                INSTRUCTION{ name: "ASL".to_string(), operate: CPU::asl, addr_mode: CPU::abs, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "BPL".to_string(), operate: CPU::bpl, addr_mode: CPU::rel, cycles: 2 }, INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::izy, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 },
                INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "ASL".to_string(), operate: CPU::asl, addr_mode: CPU::zpx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "CLC".to_string(), operate: CPU::clc, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "ORA".to_string(), operate: CPU::ora, addr_mode: CPU::abx, cycles: 4 }, INSTRUCTION{ name: "ASL".to_string(), operate: CPU::asl, addr_mode: CPU::abx, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "JSR".to_string(), operate: CPU::jsr, addr_mode: CPU::abs, cycles: 6 }, INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::izx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "BIT".to_string(), operate: CPU::bit, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "ROL".to_string(), operate: CPU::rol, addr_mode: CPU::zp0, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "PLP".to_string(), operate: CPU::plp, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::imm, cycles: 2 }, 
                INSTRUCTION{ name: "ROL".to_string(), operate: CPU::rol, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "BIT".to_string(), operate: CPU::bit, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "ROL".to_string(), operate: CPU::rol, addr_mode: CPU::abs, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "BMI".to_string(), operate: CPU::bmi, addr_mode: CPU::rel, cycles: 2 }, 
                INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::izy, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "ROL".to_string(), operate: CPU::rol, addr_mode: CPU::zpx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, 
                INSTRUCTION{ name: "SEC".to_string(), operate: CPU::sec, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "AND".to_string(), operate: CPU::and, addr_mode: CPU::abx, cycles: 4 }, INSTRUCTION{ name: "ROL".to_string(), operate: CPU::rol, addr_mode: CPU::abx, cycles: 7 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "RTI".to_string(), operate: CPU::rti, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::izx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 3 }, INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::zp0, cycles: 3 }, 
                INSTRUCTION{ name: "LSR".to_string(), operate: CPU::lsr, addr_mode: CPU::zp0, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "PHA".to_string(), operate: CPU::pha, addr_mode: CPU::imp, cycles: 3 }, INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "LSR".to_string(), operate: CPU::lsr, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "JMP".to_string(), operate: CPU::jmp, addr_mode: CPU::abs, cycles: 3 }, 
                INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "LSR".to_string(), operate: CPU::lsr, addr_mode: CPU::abs, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "BVC".to_string(), operate: CPU::bvc, addr_mode: CPU::rel, cycles: 2 }, INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::izy, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "LSR".to_string(), operate: CPU::lsr, addr_mode: CPU::zpx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "CLI".to_string(), operate: CPU::cli, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "EOR".to_string(), operate: CPU::eor, addr_mode: CPU::abx, cycles: 4 }, INSTRUCTION{ name: "LSR".to_string(), operate: CPU::lsr, addr_mode: CPU::abx, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "RTS".to_string(), operate: CPU::rts, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::izx, cycles: 6 },
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 3 }, INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "ROR".to_string(), operate: CPU::ror, addr_mode: CPU::zp0, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "PLA".to_string(), operate: CPU::pla, addr_mode: CPU::imp, cycles: 4 }, 
                INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "ROR".to_string(), operate: CPU::ror, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "JMP".to_string(), operate: CPU::jmp, addr_mode: CPU::ind, cycles: 5 }, INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "ROR".to_string(), operate: CPU::ror, addr_mode: CPU::abs, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, 
                INSTRUCTION{ name: "BVS".to_string(), operate: CPU::bvs, addr_mode: CPU::rel, cycles: 2 }, INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::izy, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "ROR".to_string(), operate: CPU::ror, addr_mode: CPU::zpx, cycles: 6 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::zpx, cycles: 6 }, INSTRUCTION{ name: "SEI".to_string(), operate: CPU::sei, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "ADC".to_string(), operate: CPU::adc, addr_mode: CPU::abx, cycles: 4 }, 
                INSTRUCTION{ name: "ROR".to_string(), operate: CPU::ror, addr_mode: CPU::abx, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "STA".to_string(), operate: CPU::sta, addr_mode: CPU::izx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "STY".to_string(), operate: CPU::sty, addr_mode: CPU::zp0, cycles: 3 }, 
                INSTRUCTION{ name: "STA".to_string(), operate: CPU::sta, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "STX".to_string(), operate: CPU::stx, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 3 }, INSTRUCTION{ name: "DEY".to_string(), operate: CPU::dey, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "TXA".to_string(), operate: CPU::txa, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, 
                INSTRUCTION{ name: "STY".to_string(), operate: CPU::sty, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "STA".to_string(), operate: CPU::sta, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "STX".to_string(), operate: CPU::stx, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "BCC".to_string(), operate: CPU::bcc, addr_mode: CPU::rel, cycles: 2 }, INSTRUCTION{ name: "STA".to_string(), operate: CPU::sta, addr_mode: CPU::izy, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "STY".to_string(), operate: CPU::sty, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "STA".to_string(), operate: CPU::sta, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "STX".to_string(), operate: CPU::stx, addr_mode: CPU::zpy, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "TYA".to_string(), operate: CPU::tya, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "STA".to_string(), operate: CPU::sta, addr_mode: CPU::aby, cycles: 5 }, 
                INSTRUCTION{ name: "TXS".to_string(), operate: CPU::txs, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "STA".to_string(), operate: CPU::sta, addr_mode: CPU::abx, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "LDY".to_string(), operate: CPU::ldy, addr_mode: CPU::imm, cycles: 2 }, 
                INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::izx, cycles: 6 }, INSTRUCTION{ name: "LDX".to_string(), operate: CPU::ldx, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "LDY".to_string(), operate: CPU::ldy, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "LDX".to_string(), operate: CPU::ldx, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 3 }, 
                INSTRUCTION{ name: "TAY".to_string(), operate: CPU::tay, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "TAX".to_string(), operate: CPU::tax, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "LDY".to_string(), operate: CPU::ldy, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "LDX".to_string(), operate: CPU::ldx, addr_mode: CPU::abs, cycles: 4 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "BCS".to_string(), operate: CPU::bcs, addr_mode: CPU::rel, cycles: 2 }, INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::izy, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "LDY".to_string(), operate: CPU::ldy, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::zpx, cycles: 4 },
                INSTRUCTION{ name: "LDX".to_string(), operate: CPU::ldx, addr_mode: CPU::zpy, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "CLV".to_string(), operate: CPU::clv, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "TSX".to_string(), operate: CPU::tsx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "LDY".to_string(), operate: CPU::ldy, addr_mode: CPU::abx, cycles: 4 }, 
                INSTRUCTION{ name: "LDA".to_string(), operate: CPU::lda, addr_mode: CPU::abx, cycles: 4 }, INSTRUCTION{ name: "LDX".to_string(), operate: CPU::ldx, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "CPY".to_string(), operate: CPU::cpy, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::izx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, 
                INSTRUCTION{ name: "CPY".to_string(), operate: CPU::cpy, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "DEC".to_string(), operate: CPU::dec, addr_mode: CPU::zp0, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "INY".to_string(), operate: CPU::iny, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "DEX".to_string(), operate: CPU::dex, addr_mode: CPU::imp, cycles: 2 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "CPY".to_string(), operate: CPU::cpy, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "DEC".to_string(), operate: CPU::dec, addr_mode: CPU::abs, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "BNE".to_string(), operate: CPU::bne, addr_mode: CPU::rel, cycles: 2 }, INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::izy, cycles: 5 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "DEC".to_string(), operate: CPU::dec, addr_mode: CPU::zpx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "CLD".to_string(), operate: CPU::cld, addr_mode: CPU::imp, cycles: 2 }, 
                INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "NOP".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "CMP".to_string(), operate: CPU::cmp, addr_mode: CPU::abx, cycles: 4 }, INSTRUCTION{ name: "DEC".to_string(), operate: CPU::dec, addr_mode: CPU::abs, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 },
                INSTRUCTION{ name: "CPX".to_string(), operate: CPU::cpx, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::izx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "CPX".to_string(), operate: CPU::cpx, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::zp0, cycles: 3 }, INSTRUCTION{ name: "INC".to_string(), operate: CPU::inc, addr_mode: CPU::zp0, cycles: 5 }, 
                INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 5 }, INSTRUCTION{ name: "INX".to_string(), operate: CPU::inx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::imm, cycles: 2 }, INSTRUCTION{ name: "NOP".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::sbc, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "CPX".to_string(), operate: CPU::cpx, addr_mode: CPU::abs, cycles: 4 }, INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::abs, cycles: 4 },
                INSTRUCTION{ name: "INC".to_string(), operate: CPU::inc, addr_mode: CPU::abs, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "BEQ".to_string(), operate: CPU::beq, addr_mode: CPU::rel, cycles: 2 }, INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::izy, cycles: 5 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 8 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, 
                INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::zpx, cycles: 4 }, INSTRUCTION{ name: "INC".to_string(), operate: CPU::inc, addr_mode: CPU::zpx, cycles: 6 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 6 }, INSTRUCTION{ name: "SED".to_string(), operate: CPU::sed, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::aby, cycles: 4 }, INSTRUCTION{ name: "NOP".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 2 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 },
                INSTRUCTION{ name: "???".to_string(), operate: CPU::nop, addr_mode: CPU::imp, cycles: 4 }, INSTRUCTION{ name: "SBC".to_string(), operate: CPU::sbc, addr_mode: CPU::abx, cycles: 4 }, INSTRUCTION{ name: "INC".to_string(), operate: CPU::inc, addr_mode: CPU::abx, cycles: 7 }, INSTRUCTION{ name: "???".to_string(), operate: CPU::xxx, addr_mode: CPU::imp, cycles: 7 },
            ],
        }
    }

    // DELTE ALL TEST FUNCS LATER
    pub fn test_lookup(&mut self, addr: u16) -> &INSTRUCTION {
        &self.lookup[addr as usize]
    }

    pub fn test_view_registers(&mut self) -> &Registers {
        &self.registers
    }

    pub fn test_view_status_reg(&mut self) -> u8 {
        self.registers.status
    }

    // Read and Write
    pub fn write(&mut self, addr: u16, data: u8) {
        self.bus.write(addr, data);
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        self.bus.read(addr)
    }

    pub fn read_with_read_only(&mut self, addr: u16, b_read_only: bool) -> u8 {
        self.bus.read_with_read_only(addr, b_read_only)
    }

    // Clock
    pub fn clock(&mut self) {
        if self.cycles == 0 {
            self.opcode = self.read(self.registers.pc);
            self.registers.pc += 1;

            // Get Starting Number of Cycles
            self.cycles = self.lookup[self.opcode as usize].cycles;

            let additional_cycle1 = (self.lookup[self.opcode as usize].addr_mode)(self);

            let additional_cycle2 = (self.lookup[self.opcode as usize].operate)(self);

            self.cycles += additional_cycle1 & additional_cycle2;


        }

        self.cycles -= 1;
    }
    

    // Set and Get flag
    pub fn get_flag(&mut self, f: FLAGS6502) -> u8 {
        if (self.registers.status & f as u8) > 0 {
            1
        } else { 
            0
        }
    }

    pub fn set_flag(&mut self, f: FLAGS6502, v: bool) {
        if v {
            self.registers.status |= f as u8;
        } else {
            self.registers.status &= !(f as u8);
        }
    }

    // Addressing Modes
    pub fn imp(&mut self) -> u8 {
        self.fetched = self.registers.a;

        0
    }

    pub fn imm(&mut self) -> u8 {
        self.addr_abs = self.registers.pc;
        self.registers.pc += 1;

        0
    }

    pub fn zp0(&mut self) -> u8 {
        self.addr_abs = (self.read(self.registers.pc)) as u16;
        self.registers.pc += 1;
        self.addr_abs &= 0x00FF;

        0
    }

    pub fn zpx(&mut self) -> u8 {
        self.addr_abs = (self.read(self.registers.pc) + self.registers.x) as u16;
        self.registers.pc += 1;
        self.addr_abs &= 0x00FF;

        0
    }

    pub fn zpy(&mut self) -> u8 {
        self.addr_abs = (self.read(self.registers.pc) + self.registers.y) as u16;
        self.registers.pc += 1;
        self.addr_abs &= 0x00FF;

        0
    }

    pub fn rel(&mut self) -> u8 {
        self.addr_rel = self.read(self.registers.pc);
        self.registers.pc += 1;

        if (self.addr_rel & 0x80) != 0 {
            self.addr_rel |= 0xFF;
        }

        0
    }

    pub fn abs(&mut self) -> u8 {
        let lo_byte = self.read(self.registers.pc);
        self.registers.pc += 1;
        let hi_byte = self.read(self.registers.pc);
        self.registers.pc += 1;

        self.addr_abs = ((hi_byte as u16) << 8) | (lo_byte as u16);

        0
    }

    pub fn abx(&mut self) -> u8 {
        let lo_byte = self.read(self.registers.pc);
        self.registers.pc += 1;
        let hi_byte = self.read(self.registers.pc);
        self.registers.pc += 1;

        self.addr_abs = ((hi_byte as u16) << 8) | (lo_byte as u16);
        self.addr_abs += self.registers.x as u16;

        if (self.addr_abs & 0xFF00) != (hi_byte as u16) << 8 {
            1
        } else {
            0
        }
    }

    pub fn aby(&mut self) -> u8 {
        let lo_byte = self.read(self.registers.pc);
        self.registers.pc += 1;
        let hi_byte = self.read(self.registers.pc);
        self.registers.pc += 1;

        self.addr_abs = ((hi_byte as u16) << 8) | (lo_byte as u16);
        self.addr_abs += self.registers.y as u16;

        if (self.addr_abs & 0xFF00) != (hi_byte as u16) << 8 {
            1
        } else {
            0
        }
    }

    pub fn ind(&mut self) -> u8 {
        let ptr_lo = self.read(self.registers.pc);
        self.registers.pc += 1;
        let ptr_hi = self.read(self.registers.pc);
        self.registers.pc += 1;

        let ptr = ((ptr_hi as u16) << 8) | (ptr_lo as u16);

        if (ptr_lo as u16) == 0x00FF { // Simulate a hardware bug, where adding one causes overflow
            self.addr_abs = ((self.read(ptr & 0xFF00) as u16) << 8) | (self.read(ptr + 0) as u16);
        } else { // Behave normally if ptr_lo is less than 0x00FF
            self.addr_abs = ((self.read(ptr + 1) as u16) << 8) | (self.read(ptr + 0) as u16);    
        }
        

        0
    }

    pub fn izx(&mut self) -> u8 {
        let t = self.read(self.registers.pc);
        self.registers.pc += 1;

        let lo = self.read(((t as u16) + (self.registers.x as u16)) & 0x00FF) as u16;
        let hi = self.read(((t as u16) + ((self.registers.x as u16) + 1)) & 0x00FF) as u16;

        self.addr_abs = (hi << 8) | lo;

        0
    }

    pub fn izy(&mut self) -> u8 {
        let t = self.read(self.registers.pc);
        self.registers.pc += 1;

        let lo = self.read((t as u16) & 0x00FF);
        let hi = self.read(((t + 1) as u16) & 0x00FF );

        self.addr_abs = (hi as u16) << 8 | (lo as u16);
        self.addr_abs += self.registers.y as u16;

        if (self.addr_abs & 0xFF00) != ((hi as u16) << 8) {
            1
        } else {
            0
        }
    }

    // Legal Opcodes
    pub fn adc (&mut self) -> u8 {
        self.fetch();

        let temp = (self.registers.a as u16) + (self.fetched as u16) + (self.get_flag(FLAGS6502::C) as u16);
        self.set_flag(FLAGS6502::C, temp > 255);
        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0);
        self.set_flag(FLAGS6502::N, (temp & 0x80) != 0);
        self.set_flag(FLAGS6502::V, !((self.registers.a as u16) ^ (self.fetched as u16)) & ((self.registers.a as u16) ^ (temp as u16)) & 0x0080 != 0);

        self.set_flag(FLAGS6502::N, (temp & 0x80) != 0);

        self.registers.a = (temp & 0x00FF) as u8;
        
        1
    }

    pub fn and (&mut self) -> u8 {
        self.fetch();

        self.registers.a = self.registers.a & self.fetched;
        self.set_flag(FLAGS6502::N, (self.registers.a & 0x80) != 0);
        self.set_flag(FLAGS6502::Z, self.registers.a == 0x00);

        1
    }

    pub fn asl (&mut self) -> u8 {
        self.fetch();
        let temp = (self.fetched as u16) << 1;
        self.set_flag(FLAGS6502::C, (temp & 0xFF00) > 0);
        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0x00);
        self.set_flag(FLAGS6502::N, (temp & 0x80) != 0);

        if self.lookup[self.opcode as usize].addr_mode == CPU::imp {
            self.registers.a = (temp & 0x00FF) as u8;
        } else {
            self.write(self.addr_abs, (temp & 0x00FF) as u8);
        }

        0
    }

    pub fn bcc (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::C) == 0 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn bcs (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::C) != 0 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn beq (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::Z) == 1 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn bit (&mut self) -> u8 {
        0
    }

    pub fn bmi (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::N) == 1 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn bne (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::Z) == 0 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn bpl (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::N) == 0 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn brk (&mut self) -> u8 {
        self.registers.pc += 1;

        self.set_flag(FLAGS6502::I, true);
        self.registers.stkp -= 1;
        self.write(0x0100 + (self.registers.stkp as u16), (self.registers.pc >> 8) as u8);
        self.registers.stkp -= 1;
        self.write(0x0100 + (self.registers.stkp as u16), self.registers.pc as u8);

        self.set_flag(FLAGS6502::B, true);
        self.write(0x100 + (self.registers.stkp as u16), self.registers.status);
        self.registers.stkp -= 1;
        self.set_flag(FLAGS6502::B, false);

        self.registers.pc = (self.read(0xFFFE) as u16) | ((self.read(0xFFFF) as u16) << 8);

        0
    }

    pub fn bvc (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::V) == 0 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn bvs (&mut self) -> u8 {
        if self.get_flag(FLAGS6502::V) == 1 {
            self.cycles += 1;
            self.addr_abs = self.registers.pc + (self.addr_rel as u16);

            if self.addr_abs & 0xFF00 != self.registers.pc & 0xFF00 {
                self.cycles += 1;
            }

            self.registers.pc = self.addr_abs;
        }

        0
    }

    pub fn clc (&mut self) -> u8 {
        self.set_flag(FLAGS6502::C, false);

        0
    }

    pub fn cld (&mut self) -> u8 {
        self.set_flag(FLAGS6502::D, false);

        0
    }

    pub fn cli (&mut self) -> u8 {
        self.set_flag(FLAGS6502::I, false);

        0
    }

    pub fn clv (&mut self) -> u8 {
        self.set_flag(FLAGS6502::V, false);
        0
    }

    pub fn cmp (&mut self) -> u8 {
        self.fetch();
        let temp = (self.registers.a as u16) - (self.fetched as u16);
        self.set_flag(FLAGS6502::C, self.registers.a >= (temp as u8));
        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0x0000);
        self.set_flag(FLAGS6502::N, (temp & 0x0080) != 0);

        1
    }

    pub fn cpx (&mut self) -> u8 {
        self.fetch();
        let temp = (self.registers.x as u16) - (self.fetched as u16);
        self.set_flag(FLAGS6502::C, self.registers.x >= (temp as u8));
        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0x0000);
        self.set_flag(FLAGS6502::N, (temp & 0x0080) != 0);

        1
    }

    pub fn cpy (&mut self) -> u8 {
        self.fetch();
        let temp = (self.registers.y as u16) - (self.fetched as u16);
        self.set_flag(FLAGS6502::C, self.registers.y >= (temp as u8));
        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0x0000);
        self.set_flag(FLAGS6502::N, (temp & 0x0080) != 0);

        1
    }

    pub fn dec (&mut self) -> u8 {
        self.fetch();
        let temp = self.fetched - 1;
        self.write(self.addr_abs, temp & 0x00FF);
        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0);
        self.set_flag(FLAGS6502::N, (temp & 0x0080) != 0);

        0
    }

    pub fn dex (&mut self) -> u8 {
        self.registers.x = self.registers.x - 1;
        self.set_flag(FLAGS6502::Z, self.registers.x == 0);
        self.set_flag(FLAGS6502::N, (self.registers.x & 0x0080) != 0);

        0
    }

    pub fn dey (&mut self) -> u8 {
        self.registers.y = self.registers.y - 1;
        self.set_flag(FLAGS6502::Z, self.registers.y == 0);
        self.set_flag(FLAGS6502::N, (self.registers.y & 0x0080) != 0);

        0
    }

    pub fn eor (&mut self) -> u8 {
        self.fetch();

        self.registers.a = self.registers.a ^ self.fetched;
        self.set_flag(FLAGS6502::Z, self.registers.a == 0);
        self.set_flag(FLAGS6502::N, (self.registers.a & 0x0080) != 0);

        1
    }

    pub fn inc (&mut self) -> u8 {
        self.fetch();

        let temp = self.fetched + 1;
        self.write(self.addr_abs, temp & 0x00FF);

        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0);
        self.set_flag(FLAGS6502::N, (temp & 0x0080) != 0);

        0
    }

    pub fn inx (&mut self) -> u8 {

        
        0
    }

    pub fn iny (&mut self) -> u8 {
        0
    }

    pub fn jmp (&mut self) -> u8 {
        0
    }

    pub fn jsr (&mut self) -> u8 {
        0
    }

    pub fn lda (&mut self) -> u8 {
        0
    }

    pub fn ldx (&mut self) -> u8 {
        0
    }

    pub fn ldy (&mut self) -> u8 {
        0
    }

    pub fn lsr (&mut self) -> u8 {
        0
    }

    pub fn nop (&mut self) -> u8 {
        0
    }

    pub fn ora (&mut self) -> u8 {
        0
    }

    pub fn pha (&mut self) -> u8 {
        self.write(0x0100 + (self.registers.stkp as u16), self.registers.a);
        self.registers.stkp -= 1;

        0
    }

    pub fn php (&mut self) -> u8 {
        0
    }

    pub fn pla (&mut self) -> u8 {
        self.registers.stkp += 1;
        self.registers.a = self.read(0x0100 + (self.registers.stkp as u16));
        self.set_flag(FLAGS6502::Z, self.registers.a == 0x00);
        self.set_flag(FLAGS6502::N, (self.registers.a & 0x80) != 0);

        0
    }

    pub fn plp (&mut self) -> u8 {
        0
    }

    pub fn rol (&mut self) -> u8 {
        0
    }

    pub fn ror (&mut self) -> u8 {
        0
    }

    pub fn rti (&mut self) -> u8 {
        self.registers.stkp += 1;
        self.registers.status = self.read(0x0100 + (self.registers.stkp as u16));
        self.registers.status &= !(FLAGS6502::B as u8);
        self.registers.status &= !(FLAGS6502::U as u8);

        self.registers.stkp += 1;
        let lo = self.read(0x0100 + (self.registers.stkp as u16)) as u16;
        self.registers.stkp += 1;
        let hi = (self.read(0x0100 + (self.registers.stkp as u16)) as u16) << 8;

        self.registers.pc = hi | lo;

        0
    }

    pub fn rts (&mut self) -> u8 {
        0
    }

    pub fn sbc (&mut self) -> u8 {
        self.fetch();

        let value = (self.fetched as u16) ^ 0x00FF;
        let temp = (self.registers.a as u16) + value + (self.get_flag(FLAGS6502::C) as u16);

        self.set_flag(FLAGS6502::C, temp > 255);
        self.set_flag(FLAGS6502::Z, (temp & 0x00FF) == 0);
        self.set_flag(FLAGS6502::N, (temp & 0x80) != 0);
        self.set_flag(FLAGS6502::V, (temp ^ (self.registers.a as u16) & (temp ^ value) & 0x0080) != 0);

        self.set_flag(FLAGS6502::N, (temp & 0x80) != 0);

        self.registers.a = (temp & 0x00FF) as u8;

        0
    }

    pub fn sec (&mut self) -> u8 {
        0
    }

    pub fn sed (&mut self) -> u8 {
        0
    }

    pub fn sei (&mut self) -> u8 {
        0
    }

    pub fn sta (&mut self) -> u8 {
        0
    }

    pub fn stx (&mut self) -> u8 {
        0
    }

    pub fn sty (&mut self) -> u8 {
        0
    }

    pub fn tax (&mut self) -> u8 {
        0
    }

    pub fn tay (&mut self) -> u8 {
        0
    }

    pub fn tsx (&mut self) -> u8 {
        0
    }

    pub fn txa (&mut self) -> u8 {
        0
    }

    pub fn txs (&mut self) -> u8 {
        0
    }

    pub fn tya (&mut self) -> u8 {
        0
    }

    // Illegal Opcodes
    pub fn xxx(&mut self) -> u8 {
        0
    }

    pub fn reset(&mut self) {
        self.registers.a = 0;
        self.registers.x = 0;
        self.registers.y = 0;
        self.registers.stkp = 0xFD;
        self.registers.status = 0x00 | (FLAGS6502::U as u8);

        self.addr_abs = 0xFFFC;
        let lo = self.read(self.addr_abs + 0);
        let hi = self.read(self.addr_abs + 1);

        self.registers.pc = ((hi as u16) << 8) | (lo as u16);

        self.addr_rel = 0x0000;
        self.addr_abs = 0x0000;
        self.fetched = 0x00;

        self.cycles = 8;
    }

    pub fn irq(&mut self) {
        if self.get_flag(FLAGS6502::I) == 0 {
            self.write(0x0100 + (self.registers.stkp as u16), (self.registers.pc >> 8) as u8);
            self.registers.stkp -= 1;
            self.write(0x0100 + (self.registers.stkp as u16), (self.registers.pc & 0x00FF) as u8);
            self.registers.stkp -= 1;

            self.set_flag(FLAGS6502::B, false);
            self.set_flag(FLAGS6502::U, true);
            self.set_flag(FLAGS6502::I, true);
            self.write(0x0100 + (self.registers.stkp as u16), self.registers.status);
            self.registers.stkp -= 1;

            self.addr_abs = 0xFFFE;
            let lo = self.read(self.addr_abs + 0);
            let hi = self.read(self.addr_abs + 1);

            self.registers.pc = ((hi as u16) << 8) | (lo as u16);

            self.cycles = 7;
        }
    }

    pub fn nmi(&mut self) {
        self.write(0x0100 + (self.registers.stkp as u16), (self.registers.pc >> 8) as u8);
        self.registers.stkp -= 1;
        self.write(0x0100 + (self.registers.stkp as u16), (self.registers.pc & 0x00FF) as u8);
        self.registers.stkp -= 1;

        self.set_flag(FLAGS6502::B, false);
        self.set_flag(FLAGS6502::U, true);
        self.set_flag(FLAGS6502::I, true);
        self.write(0x0100 + (self.registers.stkp as u16), self.registers.status);
        self.registers.stkp -= 1;

        self.addr_abs = 0xFFFA;
        let lo = self.read(self.addr_abs + 0);
        let hi = self.read(self.addr_abs + 1);

        self.registers.pc = ((hi as u16) << 8) | (lo as u16);

        self.cycles = 8;
    }

    // Fetch and add to fetched
    pub fn fetch(&mut self) -> u8 {
        if !(self.lookup[self.opcode as usize].addr_mode == CPU::imp) {
            self.fetched = self.read(self.addr_abs);
        }

        self.fetched
    }
}


// For status register
pub enum FLAGS6502 {
    C = (1 << 0), // Carry Bit
    Z = (1 << 1), // Zero
    I = (1 << 2), // Disable Interrupts
    D = (1 << 3), // Decimal Mode (Not using in this implementation, since nes doesn't use it)
    B = (1 << 4), // Break
    U = (1 << 5), // Unused
    V = (1 << 6), // Overflow
    N = (1 << 7), // Negative
}

#[derive(Debug)]
pub struct Registers {
    a: u8,
    x: u8,
    y: u8,
    stkp: u8,
    pc: u16,
    status: u8,
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            a: 0x00,
            x: 0x00,
            y: 0x00,
            stkp: 0x00,
            pc: 0x0000,
            status: 0x00,
        }
    }
}

#[derive(Debug)]
pub struct INSTRUCTION {
    name: String,
    operate: fn(&mut CPU) -> u8,
    addr_mode: fn(&mut CPU) -> u8,
    cycles: u8,
}