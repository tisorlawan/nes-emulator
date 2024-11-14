const std = @import("std");
const expect = std.testing.expect;

pub const AddrMode = enum {
    Implicit,
    /// Immediate addressing uses the next byte as the operand value directly
    /// Example: LDA #$05 - Load the value $05 into accumulator
    Immediate,

    Relative,

    /// Zero Page addressing uses the next byte as an address in page zero ($0000-$00FF)
    /// Example: LDA $05 - Load the value from memory location $0005
    ZeroPage,

    /// Zero Page,X addressing adds X register to zero page address
    /// Example: LDA $05,X - Load value from ($0005 + X)
    ZeroPageX,

    /// Zero Page,Y addressing adds Y register to zero page address
    /// Example: LDA $05,Y - Load value from ($0005 + Y)
    ZeroPageY,

    /// Absolute addressing uses next two bytes as full memory address
    /// Example: LDA $1234 - Load value from memory location $1234
    Absolute,

    /// Absolute,X addressing adds X register to the absolute address
    /// Example: LDA $1234,X - Load value from ($1234 + X)
    AbsoluteX,

    /// Absolute,Y addressing adds Y register to the absolute address
    /// Example: LDA $1234,Y - Load value from ($1234 + Y)
    AbsoluteY,

    /// Indirect,X addressing: Zero page address is added to X, result used to fetch effective address
    /// Example: LDA ($20,X) - Get address from ($20 + X) and ($20 + X + 1), load value from there
    IndirectX,

    /// Indirect,Y addressing: Zero page address points to base address, Y is added to that
    /// Example: LDA ($20),Y - Get address from $20 and $21, add Y to it, load value from there
    IndirectY,

    /// Used for implied addressing where no operand is needed
    None,
};

pub const Status = enum(u8) {
    Carry = 0b0000_0001,
    Zero = 0b0000_0010,
    InterruptDisable = 0b0000_0100,
    DecimalMode = 0b0000_1000,
    Break = 0b0001_0000,
    Break2 = 0b0010_0000,
    Overflow = 0b0100_0000,
    Negative = 0b1000_0000,
};

pub const OpCode = struct {
    code: u8, // Opcode
    mnemonic: []const u8, // instruction name
    len: u8, // number of bytes
    cycles: u8, // number of cycles
    mode: AddrMode, // address mode
    cross_penalty: u8, // + if crossed
};

pub const BRK = 0x00;
pub const NOP = 0xEA;

pub const ADC_IMM = 0x69;
pub const ADC_ZP = 0x65;
pub const ADC_ZPX = 0x75;
pub const ADC_ABS = 0x6D;
pub const ADC_ABSX = 0x7D;
pub const ADC_ABSY = 0x79;
pub const ADC_INDX = 0x61;
pub const ADC_INDY = 0x71;

pub const AND_IMM = 0x29;
pub const AND_ZP = 0x25;
pub const AND_ZPX = 0x35;
pub const AND_ABS = 0x2D;
pub const AND_ABSX = 0x3D;
pub const AND_ABSY = 0x39;
pub const AND_INDX = 0x21;
pub const AND_INDY = 0x31;

pub const ASL = 0x0A;
pub const ASL_ZP = 0x06;
pub const ASL_ZPX = 0x16;
pub const ASL_ABS = 0x0E;
pub const ASL_ABSX = 0x1E;

pub const BCC = 0x90; // Branch if Carry Clear
pub const BCS = 0xB0; // Branch if Carry Set
pub const BEQ = 0xF0; // Branch if Equal (if zero flag is set)

pub const BIT_ZP = 0x24; // Bit Test Zero Page
pub const BIT_ABS = 0x2C; // Bit Test Absolute

pub const BMI = 0x30; // Branch on MInus
pub const BNE = 0xD0; // Branch on Not Equal
pub const BPL = 0x10; // Branch on Positive
pub const BVC = 0x50; // Branch on Overflow Clear
pub const BVS = 0x70; // Branch on Overflow Set

pub const CLC = 0x18; // CLear Carry Flag
pub const CLD = 0xD8; // CLear Decimal Mode
pub const CLI = 0x58; // CLear Decimal Interupt Disable Flag
pub const CLV = 0xB8; // CLear oVerflow Flag

// Compare instructions
pub const CMP_IMM = 0xC9;
pub const CMP_ZP = 0xC5;
pub const CMP_ZPX = 0xD5;
pub const CMP_ABS = 0xCD;
pub const CMP_ABSX = 0xDD;
pub const CMP_ABSY = 0xD9;
pub const CMP_INDX = 0xC1;
pub const CMP_INDY = 0xD1;

pub const CPX_IMM = 0xE0;
pub const CPX_ZP = 0xE4;
pub const CPX_ABS = 0xEC;

pub const CPY_IMM = 0xC0;
pub const CPY_ZP = 0xC4;
pub const CPY_ABS = 0xCC;

// Opcode constants for DEC (Decrement Memory)
pub const DEC_ZP = 0xC6;
pub const DEC_ZPX = 0xD6;
pub const DEC_ABS = 0xCE;
pub const DEC_ABSX = 0xDE;

// Opcode constants for DEX and DEY (Decrement X and Y Registers)
pub const DEX = 0xCA;
pub const DEY = 0x88;

// ExclusiveOR
pub const EOR_IMM = 0x49;
pub const EOR_ZP = 0x45;
pub const EOR_ZPX = 0x55;
pub const EOR_ABS = 0x4D;
pub const EOR_ABSX = 0x5D;
pub const EOR_ABSY = 0x59;
pub const EOR_INDX = 0x41;
pub const EOR_INDY = 0x51;

// INCrement
pub const INC_ZP = 0xE6;
pub const INC_ZPX = 0xF6;
pub const INC_ABS = 0xEE;
pub const INC_ABSX = 0xFE;

pub const INX = 0xE8;
pub const INY = 0xC8;

// JuMP
pub const JMP_ABS = 0x4C;
pub const JMP_IND = 0x6C;

// pub const JSR_ABS = 0x20;

// LoaD Accumulator
pub const LDA_IMM = 0xA9;
pub const LDA_ZP = 0xA5;
pub const LDA_ZPX = 0xB5;
pub const LDA_ABS = 0xAD;
pub const LDA_ABSX = 0xBD;
pub const LDA_ABSY = 0xB9;
pub const LDA_INDX = 0xA1;
pub const LDA_INDY = 0xB1;

// LoaD X Register
pub const LDX_IMM = 0xA2;
pub const LDX_ZP = 0xA6;
pub const LDX_ZPY = 0xB6;
pub const LDX_ABS = 0xAE;
pub const LDX_ABSY = 0xBE;

// LoaD Y Register
pub const LDY_IMM = 0xA0;
pub const LDY_ZP = 0xA4;
pub const LDY_ZPX = 0xB4;
pub const LDY_ABS = 0xAC;
pub const LDY_ABSX = 0xBC;

pub const TAX = 0xAA; // Transfer Accumulator to X

pub const PHA = 0x48; // PusH Accumulator
pub const PHP = 0x08; // PusH Processor Status
pub const PLA = 0x68; // PulL Accumulator
pub const PLP = 0x28; // PulL Processor Status

pub const SEC = 0x38; // SEt Carry Flag
pub const SED = 0xF8; // SEt Decimal Flag
pub const SEI = 0x78; // SEt Interrupt Disable Flag

// STore Accumulator
pub const STA_ZP = 0x85;
pub const STA_ZPX = 0x95;
pub const STA_ABS = 0x8D;
pub const STA_ABSX = 0x9D;
pub const STA_ABSY = 0x99;
pub const STA_INDX = 0x81;
pub const STA_INDY = 0x91;

const opCodes = [_]OpCode{
    .{ .code = BRK, .mnemonic = "BRK", .len = 1, .cycles = 7, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = NOP, .mnemonic = "NOP", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = ADC_ABS, .mnemonic = "ADC", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = ADC_ABSX, .mnemonic = "ADC", .len = 3, .cycles = 4, .mode = .AbsoluteX, .cross_penalty = 1 },
    .{ .code = ADC_ABSY, .mnemonic = "ADC", .len = 3, .cycles = 4, .mode = .AbsoluteY, .cross_penalty = 1 },
    .{ .code = ADC_IMM, .mnemonic = "ADC", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = ADC_INDX, .mnemonic = "ADC", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = ADC_INDY, .mnemonic = "ADC", .len = 2, .cycles = 5, .mode = .IndirectY, .cross_penalty = 1 },
    .{ .code = ADC_ZP, .mnemonic = "ADC", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = ADC_ZPX, .mnemonic = "ADC", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },

    .{ .code = AND_IMM, .mnemonic = "AND", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = AND_ZP, .mnemonic = "AND", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = AND_ZPX, .mnemonic = "AND", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },
    .{ .code = AND_ABS, .mnemonic = "AND", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = AND_ABSX, .mnemonic = "AND", .len = 3, .cycles = 4, .mode = .AbsoluteX, .cross_penalty = 1 },
    .{ .code = AND_ABSY, .mnemonic = "AND", .len = 3, .cycles = 4, .mode = .AbsoluteY, .cross_penalty = 1 },
    .{ .code = AND_INDX, .mnemonic = "AND", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = AND_INDY, .mnemonic = "AND", .len = 2, .cycles = 5, .mode = .IndirectY, .cross_penalty = 1 },

    .{ .code = ASL, .mnemonic = "ASL", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = ASL_ZP, .mnemonic = "ASL", .len = 2, .cycles = 5, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = ASL_ZPX, .mnemonic = "ASL", .len = 2, .cycles = 6, .mode = .ZeroPageX, .cross_penalty = 0 },
    .{ .code = ASL_ABS, .mnemonic = "ASL", .len = 3, .cycles = 6, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = ASL_ABSX, .mnemonic = "ASL", .len = 3, .cycles = 7, .mode = .AbsoluteX, .cross_penalty = 0 },

    .{ .code = BCC, .mnemonic = "BCC", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 },
    .{ .code = BCS, .mnemonic = "BCS", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 },
    .{ .code = BEQ, .mnemonic = "BEQ", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 }, // +1 if branch taken, +2 if page boundary crossed

    .{ .code = BIT_ZP, .mnemonic = "BIT", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = BIT_ABS, .mnemonic = "BIT", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },

    .{ .code = BMI, .mnemonic = "BMI", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 },
    .{ .code = BNE, .mnemonic = "BNE", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 },
    .{ .code = BPL, .mnemonic = "BPL", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 },
    .{ .code = BVC, .mnemonic = "BVC", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 },
    .{ .code = BVS, .mnemonic = "BVS", .len = 2, .cycles = 2, .mode = .Relative, .cross_penalty = 1 },

    .{ .code = CLC, .mnemonic = "CLC", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = CLD, .mnemonic = "CLD", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = CLI, .mnemonic = "CLI", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = CLV, .mnemonic = "CLV", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = CMP_IMM, .mnemonic = "CMP", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = CMP_ZP, .mnemonic = "CMP", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = CMP_ZPX, .mnemonic = "CMP", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },
    .{ .code = CMP_ABS, .mnemonic = "CMP", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = CMP_ABSX, .mnemonic = "CMP", .len = 3, .cycles = 4, .mode = .AbsoluteX, .cross_penalty = 1 },
    .{ .code = CMP_ABSY, .mnemonic = "CMP", .len = 3, .cycles = 4, .mode = .AbsoluteY, .cross_penalty = 1 },
    .{ .code = CMP_INDX, .mnemonic = "CMP", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = CMP_INDY, .mnemonic = "CMP", .len = 2, .cycles = 5, .mode = .IndirectY, .cross_penalty = 1 },

    .{ .code = CPX_IMM, .mnemonic = "CPX", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = CPX_ZP, .mnemonic = "CPX", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = CPX_ABS, .mnemonic = "CPX", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },

    .{ .code = CPY_IMM, .mnemonic = "CPY", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = CPY_ZP, .mnemonic = "CPY", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = CPY_ABS, .mnemonic = "CPY", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },

    .{ .code = DEC_ZP, .mnemonic = "DEC", .len = 2, .cycles = 5, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = DEC_ZPX, .mnemonic = "DEC", .len = 2, .cycles = 6, .mode = .ZeroPageX, .cross_penalty = 0 },
    .{ .code = DEC_ABS, .mnemonic = "DEC", .len = 3, .cycles = 6, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = DEC_ABSX, .mnemonic = "DEC", .len = 3, .cycles = 7, .mode = .AbsoluteX, .cross_penalty = 0 },
    .{ .code = DEX, .mnemonic = "DEX", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = DEY, .mnemonic = "DEY", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = EOR_IMM, .mnemonic = "EOR", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = EOR_ZP, .mnemonic = "EOR", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = EOR_ZPX, .mnemonic = "EOR", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },
    .{ .code = EOR_ABS, .mnemonic = "EOR", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = EOR_ABSX, .mnemonic = "EOR", .len = 3, .cycles = 4, .mode = .AbsoluteX, .cross_penalty = 1 },
    .{ .code = EOR_ABSY, .mnemonic = "EOR", .len = 3, .cycles = 4, .mode = .AbsoluteY, .cross_penalty = 1 },
    .{ .code = EOR_INDX, .mnemonic = "EOR", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = EOR_INDY, .mnemonic = "EOR", .len = 2, .cycles = 5, .mode = .IndirectY, .cross_penalty = 1 },

    .{ .code = INC_ZP, .mnemonic = "INC", .len = 2, .cycles = 5, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = INC_ZPX, .mnemonic = "INC", .len = 2, .cycles = 6, .mode = .ZeroPageX, .cross_penalty = 0 },
    .{ .code = INC_ABS, .mnemonic = "INC", .len = 3, .cycles = 6, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = INC_ABSX, .mnemonic = "INC", .len = 3, .cycles = 7, .mode = .AbsoluteX, .cross_penalty = 0 },
    .{ .code = INX, .mnemonic = "INX", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = INY, .mnemonic = "INY", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = JMP_ABS, .mnemonic = "JMP", .len = 3, .cycles = 3, .mode = .None, .cross_penalty = 0 },
    .{ .code = JMP_IND, .mnemonic = "JMP", .len = 3, .cycles = 5, .mode = .None, .cross_penalty = 0 },

    // .{ .code = JSR_ABS, .mnemonic = "JSR", .len = 3, .cycles = 6, .mode = .Absolute, .cross_penalty = 0 },

    .{ .code = LDA_ABS, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = LDA_ABSX, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteX, .cross_penalty = 1 },
    .{ .code = LDA_ABSY, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteY, .cross_penalty = 1 },
    .{ .code = LDA_IMM, .mnemonic = "LDA", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = LDA_INDX, .mnemonic = "LDA", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = LDA_INDY, .mnemonic = "LDA", .len = 2, .cycles = 5, .mode = .IndirectY, .cross_penalty = 1 },
    .{ .code = LDA_ZP, .mnemonic = "LDA", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = LDA_ZPX, .mnemonic = "LDA", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },

    .{ .code = LDX_IMM, .mnemonic = "LDX", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = LDX_ZP, .mnemonic = "LDX", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = LDX_ZPY, .mnemonic = "LDX", .len = 2, .cycles = 4, .mode = .ZeroPageY, .cross_penalty = 0 },
    .{ .code = LDX_ABS, .mnemonic = "LDX", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = LDX_ABSY, .mnemonic = "LDX", .len = 3, .cycles = 4, .mode = .AbsoluteY, .cross_penalty = 1 },

    .{ .code = LDY_IMM, .mnemonic = "LDY", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = LDY_ZP, .mnemonic = "LDY", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = LDY_ZPX, .mnemonic = "LDY", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },
    .{ .code = LDY_ABS, .mnemonic = "LDY", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = LDY_ABSX, .mnemonic = "LDY", .len = 3, .cycles = 4, .mode = .AbsoluteX, .cross_penalty = 1 },

    .{ .code = SEC, .mnemonic = "SEC", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = SED, .mnemonic = "SED", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = SEI, .mnemonic = "SEI", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = STA_ABS, .mnemonic = "STA", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = STA_ABSX, .mnemonic = "STA", .len = 3, .cycles = 5, .mode = .AbsoluteX, .cross_penalty = 0 },
    .{ .code = STA_ABSY, .mnemonic = "STA", .len = 3, .cycles = 5, .mode = .AbsoluteY, .cross_penalty = 0 },
    .{ .code = STA_INDX, .mnemonic = "STA", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = STA_INDY, .mnemonic = "STA", .len = 2, .cycles = 6, .mode = .IndirectY, .cross_penalty = 0 },
    .{ .code = STA_ZP, .mnemonic = "STA", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = STA_ZPX, .mnemonic = "STA", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },

    .{ .code = TAX, .mnemonic = "TAX", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = PHA, .mnemonic = "PHA", .len = 1, .cycles = 3, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = PHP, .mnemonic = "PHP", .len = 1, .cycles = 3, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = PLA, .mnemonic = "PLA", .len = 1, .cycles = 4, .mode = .Implicit, .cross_penalty = 0 },
    .{ .code = PLP, .mnemonic = "PLP", .len = 1, .cycles = 4, .mode = .Implicit, .cross_penalty = 0 },
};

pub const OpcodeMap = struct {
    const lookup_table = blk: {
        var table: [256]?OpCode = [_]?OpCode{null} ** 256;
        for (opCodes) |opcode| {
            table[opcode.code] = opcode;
        }
        break :blk table;
    };

    pub fn get(code: u8) ?OpCode {
        return lookup_table[code];
    }
};

const CPU = struct {
    memory: [0xFFFF]u8 = .{0} ** 0xFFFF,
    status: u8 = 0,
    pc: u16 = 0,

    // registers
    a: u8 = 0,
    x: u8 = 0,
    y: u8 = 0,

    // stack related
    sp: u8 = 0xFF,

    const PROG_ROM_START_ADDR = 0x8000;
    const ADDR_START = 0xFFFC;
    const STACK_START: u16 = 0x0100;

    fn memRead(self: *CPU, addr: u16) u8 {
        return self.memory[addr];
    }

    fn memReadU16(self: *CPU, addr: u16) u16 {
        const lo = self.memRead(addr);
        const hi = self.memRead(addr + 1);
        return (@as(u16, hi) << 8) | lo;
    }

    fn memWrite(self: *CPU, addr: u16, value: u8) void {
        self.memory[addr] = value;
    }

    fn memWriteU16(self: *CPU, addr: u16, value: u16) void {
        const hi: u8 = @truncate(value >> 8);
        const lo: u8 = @truncate(value & 0x00FF);
        self.memWrite(addr, lo);
        self.memWrite(addr +% 1, hi);
    }

    pub fn reset(self: *CPU) void {
        self.a = 0;
        self.x = 0;
        self.status = 0;
        self.pc = self.memReadU16(ADDR_START);
    }

    pub fn load(self: *CPU, program: []const u8) void {
        std.mem.copyForwards(u8, self.memory[PROG_ROM_START_ADDR..][0..program.len], program);
        self.pc = PROG_ROM_START_ADDR;
        self.memWriteU16(ADDR_START, PROG_ROM_START_ADDR);
    }

    pub fn loadAndRun(self: *CPU, program: []const u8) void {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn statusSet(self: *CPU, status: Status) void {
        self.status |= @intFromEnum(status);
    }

    pub fn statusClear(self: *CPU, status: Status) void {
        self.status &= (~@intFromEnum(status));
    }

    pub fn statusHas(self: *CPU, status: Status) bool {
        return (self.status & @intFromEnum(status)) != 0;
    }

    pub fn statusSetCond(self: *CPU, status: Status, condition: bool) void {
        if (condition) {
            self.statusSet(status);
        } else {
            self.statusClear(status);
        }
    }

    pub fn push(self: *CPU, data: u8) void {
        self.memory[STACK_START + @as(u16, self.sp)] = data;
        self.sp -%= 1;
    }

    pub fn pull(self: *CPU) u8 {
        self.sp +%= 1;
        return self.memory[STACK_START + @as(u16, self.sp)];
    }

    pub fn run(self: *CPU) void {
        while (true) {
            const opcode = self.memRead(self.pc);
            self.pc += 1;

            const initial_pc = self.pc;

            switch (opcode) {
                ADC_IMM, ADC_ZP, ADC_ZPX, ADC_ABS, ADC_ABSX, ADC_ABSY, ADC_INDX, ADC_INDY => {
                    self.adc(OpcodeMap.get(opcode).?.mode);
                },
                AND_IMM, AND_ZP, AND_ZPX, AND_ABS, AND_ABSX, AND_ABSY, AND_INDX, AND_INDY => {
                    self.and_(OpcodeMap.get(opcode).?.mode);
                },
                ASL, ASL_ZP, ASL_ZPX, ASL_ABS, ASL_ABSX => {
                    _ = self.asl(OpcodeMap.get(opcode).?.mode, opcode == ASL);
                },
                BCC => self.branch(!self.statusHas(.Carry)),
                BCS => self.branch(self.statusHas(.Carry)),
                BEQ => self.branch(self.statusHas(.Zero)),
                BIT_ABS, BIT_ZP => self.bit(OpcodeMap.get(opcode).?.mode),
                BMI => self.branch(self.statusHas(.Negative)),
                BNE => self.branch(!self.statusHas(.Zero)),
                BPL => self.branch(!self.statusHas(.Negative)),
                BRK => break,
                BVC => self.branch(!self.statusHas(.Overflow)),
                BVS => self.branch(self.statusHas(.Overflow)),

                CLC => self.statusClear(.Carry),
                CLD => self.statusClear(.DecimalMode),
                CLI => self.statusClear(.InterruptDisable),
                CLV => self.statusClear(.Overflow),

                CMP_IMM, CMP_ZP, CMP_ZPX, CMP_ABS, CMP_ABSX, CMP_ABSY, CMP_INDX, CMP_INDY => self.cmp(self.a, OpcodeMap.get(opcode).?.mode),
                CPX_IMM, CPX_ZP, CPX_ABS => self.cmp(self.x, OpcodeMap.get(opcode).?.mode),
                CPY_IMM, CPY_ZP, CPY_ABS => self.cmp(self.y, OpcodeMap.get(opcode).?.mode),

                DEC_ZP, DEC_ZPX, DEC_ABS, DEC_ABSX => self.dec(OpcodeMap.get(opcode).?.mode),
                DEX => self.dex(),
                DEY => self.dey(),

                EOR_IMM, EOR_ZP, EOR_ZPX, EOR_ABS, EOR_ABSX, EOR_ABSY, EOR_INDX, EOR_INDY => self.eor(OpcodeMap.get(opcode).?.mode),

                INC_ZP, INC_ZPX, INC_ABS, INC_ABSX => self.inc(OpcodeMap.get(opcode).?.mode),
                INX => self.inx(),
                INY => self.iny(),

                JMP_ABS, JMP_IND => self.jmp(OpcodeMap.get(opcode).?.mode),
                // JSR_ABS => self.jsr(),

                LDA_IMM, LDA_ZP, LDA_ZPX, LDA_ABS, LDA_ABSX, LDA_ABSY, LDA_INDX, LDA_INDY => {
                    self.lda(OpcodeMap.get(opcode).?.mode);
                },
                LDX_IMM, LDX_ZP, LDX_ZPY, LDX_ABS, LDX_ABSY => self.ldx(OpcodeMap.get(opcode).?.mode),
                LDY_IMM, LDY_ZP, LDY_ZPX, LDY_ABS, LDY_ABSX => self.ldy(OpcodeMap.get(opcode).?.mode),

                NOP => {},

                PHA => self.pha(),
                PHP => self.php(),
                PLA => self.pla(),
                PLP => self.plp(),

                SEC => self.statusSet(.Carry),
                SED => self.statusSet(.DecimalMode),
                SEI => self.statusSet(.InterruptDisable),

                STA_ZP, STA_ZPX, STA_ABS, STA_ABSX, STA_ABSY, STA_INDX, STA_INDY => {
                    self.sta(OpcodeMap.get(opcode).?.mode);
                },

                TAX => self.tax(),
                else => unreachable,
            }

            if (initial_pc == self.pc) {
                self.pc += OpcodeMap.get(opcode).?.len - 1;
            }
        }
    }

    fn adc(self: *CPU, mode: AddrMode) void {
        const value = self.memRead(self.get_op_addr(mode));
        const carry_in = self.status & 0b0000_0001;
        const result: u16 = @as(u16, value) + @as(u16, self.a) + carry_in;

        // Carry
        if (result > 0xFF) {
            self.statusSet(.Carry);
        } else {
            self.statusClear(.Carry);
        }

        // Overflow
        // - both operands are positive but the result is negative, OR
        // - both operands are negative but the result is positive
        const a_sign = (self.a & 0x80) != 0;
        const m_sign = (value & 0x80) != 0;
        const r_sign = (result & 0x80) != 0;

        if ((a_sign == m_sign) and (a_sign != r_sign)) {
            self.statusSet(.Overflow);
        } else {
            self.statusClear(.Overflow);
        }

        self.a = @truncate(result);
        self.update_zero_and_negative_flag(self.a);
    }

    fn and_(self: *CPU, mode: AddrMode) void {
        const value = self.memRead(self.get_op_addr(mode));
        self.a &= value;
        self.update_zero_and_negative_flag(self.a);
    }

    fn asl(self: *CPU, mode: AddrMode, for_accumulator: bool) u8 {
        if (for_accumulator) {
            var data = self.a;
            if (data >> 7 == 1) {
                self.statusSet(.Carry);
            } else {
                self.statusClear(.Carry);
            }
            data = data << 1;
            self.a = data;
            self.update_zero_and_negative_flag(self.a);
            return data;
        } else {
            const addr = self.get_op_addr(mode);
            var data = self.memRead(addr);
            if (data >> 7 == 1) {
                self.statusSet(.Carry);
            } else {
                self.statusClear(.Carry);
            }
            data = data << 1;
            self.memWrite(addr, data);
            self.update_zero_and_negative_flag(data);
            return data;
        }
    }

    fn bit(self: *CPU, mode: AddrMode) void {
        const value = self.memRead(self.get_op_addr(mode));
        const and_result = self.a & value;
        self.statusSetCond(.Zero, and_result == 0);

        const n = (value & 0b1000_0000);
        self.statusSetCond(.Negative, n > 0);
        const v = (value & 0b0100_0000);
        self.statusSetCond(.Overflow, v > 0);
    }

    fn cmp(self: *CPU, value: u8, mode: AddrMode) void {
        const m = self.memRead(self.get_op_addr(mode));
        self.statusSetCond(.Carry, value >= m);
        self.statusSetCond(.Zero, value == m);
        self.statusSetCond(.Negative, (value -% m) & (0x80) != 0);
    }

    fn dec(self: *CPU, mode: AddrMode) void {
        const m_addr = self.get_op_addr(mode);
        const m_value = self.memRead(m_addr);
        const result = m_value -% 1;
        self.update_zero_and_negative_flag(result);
        self.memWrite(m_addr, result);
    }

    fn dex(self: *CPU) void {
        const result = self.x -% 1;
        self.update_zero_and_negative_flag(result);
        self.x = result;
    }

    fn dey(self: *CPU) void {
        const result = self.y -% 1;
        self.update_zero_and_negative_flag(result);
        self.y = result;
    }

    fn eor(self: *CPU, mode: AddrMode) void {
        const result = self.a ^ self.memRead(self.get_op_addr(mode));
        self.update_zero_and_negative_flag(result);
        self.a = result;
    }

    fn inc(self: *CPU, mode: AddrMode) void {
        const m_addr = self.get_op_addr(mode);
        const m_value = self.memRead(m_addr);
        const result = m_value +% 1;
        self.update_zero_and_negative_flag(result);
        self.memWrite(m_addr, result);
    }

    fn inx(self: *CPU) void {
        self.x +%= 1;
        self.update_zero_and_negative_flag(self.x);
    }

    fn iny(self: *CPU) void {
        self.y +%= 1;
        self.update_zero_and_negative_flag(self.y);
    }

    fn jmp(self: *CPU, mode: AddrMode) void {
        const addr = self.memReadU16(self.pc);
        if (mode == .Absolute) {
            self.pc = addr;
        } else {
            // An original 6502 has does not correctly fetch the target address
            // if the indirect vector falls on a page boundary (e.g. $xxFF where xx is any value from $00 to $FF).
            // In this case fetches the LSB from $xxFF as expected but takes the MSB from $xx00.
            if (addr & 0x00FF == 0x00FF) {
                const lo = self.memRead(addr);
                const hi = self.memRead(addr & 0xFF00);
                self.pc = (@as(u16, hi) << 8) | @as(u16, lo);
            } else {
                self.pc = self.memReadU16(addr);
            }
        }
    }

    fn lda(self: *CPU, mode: AddrMode) void {
        const value = self.memRead(self.get_op_addr(mode));
        self.a = value;
        self.update_zero_and_negative_flag(self.a);
    }

    fn ldx(self: *CPU, mode: AddrMode) void {
        const value = self.memRead(self.get_op_addr(mode));
        self.x = value;
        self.update_zero_and_negative_flag(self.x);
    }

    fn ldy(self: *CPU, mode: AddrMode) void {
        const value = self.memRead(self.get_op_addr(mode));
        self.y = value;
        self.update_zero_and_negative_flag(self.y);
    }

    fn pha(self: *CPU) void {
        self.push(self.a);
    }

    fn php(self: *CPU) void {
        //http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
        self.push(self.status | 0b0011_0000);
    }

    fn pla(self: *CPU) void {
        self.a = self.pull();
        self.update_zero_and_negative_flag(self.a);
    }

    fn plp(self: *CPU) void {
        self.status = self.pull();
        self.statusClear(.Break);
        self.statusSet(.Break2);
    }

    fn sta(self: *CPU, mode: AddrMode) void {
        const addr = self.get_op_addr(mode);
        self.memWrite(addr, self.a);
    }

    fn tax(self: *CPU) void {
        self.x = self.a;
        self.update_zero_and_negative_flag(self.x);
    }

    fn get_op_addr(self: *CPU, addrMode: AddrMode) u16 {
        return switch (addrMode) {
            AddrMode.Immediate => self.pc,
            AddrMode.ZeroPage => self.memRead(self.pc),
            AddrMode.Absolute => self.memReadU16(self.pc),
            AddrMode.ZeroPageX => self.memRead(self.pc) +% self.x,
            AddrMode.ZeroPageY => self.memRead(self.pc) +% self.y,
            AddrMode.AbsoluteX => self.memReadU16(self.pc) +% self.x,
            AddrMode.AbsoluteY => self.memReadU16(self.pc) +% self.y,
            AddrMode.IndirectX => blk: {
                const ptr: u8 = self.memRead(self.pc) +% self.x;
                const lo: u8 = self.memRead(ptr);
                const hi: u8 = self.memRead(ptr +% 1);
                break :blk (@as(u16, hi) << 8) | lo;
            },
            AddrMode.IndirectY => blk: {
                const ptr: u8 = self.memRead(self.pc);
                const lo: u8 = self.memRead(ptr);
                const hi: u8 = self.memRead(ptr +% 1);
                const deref = (@as(u16, hi) << 8) | lo;
                break :blk (deref +% self.y);
            },
            AddrMode.None, AddrMode.Implicit, AddrMode.Relative => unreachable,
        };
    }

    fn update_zero_and_negative_flag(self: *CPU, result: u8) void {
        if (result == 0) {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }

        if (result & 0b1000_0000 != 0) {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }

    fn branch(self: *CPU, condition: bool) void {
        if (condition) {
            const jump: i8 = @bitCast(self.memRead(self.pc));
            self.pc = self.pc +% 1 +% @as(u16, @intCast(@as(i16, jump)));
        }
    }
};

test "NOP" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ NOP, NOP, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
}

test "ADC" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x50, ADC_IMM, 0x50, BRK });
    try expect(cpu.a == 0xA0); // 0b1010_0000
    try expect(cpu.statusHas(.Negative) == true);
    try expect(cpu.statusHas(.Carry) == false);
    try expect(cpu.statusHas(.Zero) == false);
    // overflow
    try expect(cpu.statusHas(.Overflow) == true);

    // carry flag
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0xFF, ADC_IMM, 0x01, BRK });
    try expect(cpu.a == 0x00);
    try expect(cpu.statusHas(.Carry));
    try expect(cpu.statusHas(.Zero));

    // zero page
    cpu.memWrite(0x42, 0x01);
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0xFF, ADC_ZP, 0x42, BRK });
    try expect(cpu.a == 0x00);
    try expect(cpu.statusHas(.Carry));
    try expect(cpu.statusHas(.Zero));

    // zero page + x
    cpu.memWrite(0x42, 0x01);
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x10, TAX, LDA_IMM, 0xFF, ADC_ZPX, 0x32, BRK });
    try expect(cpu.a == 0x00);
    try expect(cpu.statusHas(.Carry));
    try expect(cpu.statusHas(.Zero));

    // abs
    cpu.memWrite(0x4269, 0x01);
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0xFF, ADC_ABS, 0x69, 0x42, BRK });
    try expect(cpu.a == 0x00);
    try expect(cpu.statusHas(.Carry));
    try expect(cpu.statusHas(.Zero));

    // with sec
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x42, ADC_IMM, 0x26, BRK });
    try expect(cpu.a == 0x68);
    try expect(cpu.statusHas(.Carry) == false);
    cpu.loadAndRun(&[_]u8{ SEC, LDA_IMM, 0x42, ADC_IMM, 0x26, BRK });

    try expect(cpu.a == 0x69);
    try expect(cpu.statusHas(.Carry) == false);
}

test "AND" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0b1001_0101, AND_IMM, 0b1001_0011 });
    try expect(cpu.a == 0b1001_0001);
    try expect(cpu.statusHas(.Negative));
}

test "ASL" {
    var cpu = CPU{};

    // accumulator
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x55, ASL, BRK });
    try expect(cpu.a == 0xAA); // 0b0101_0101 -> 0b1010_1010
    try expect(cpu.statusHas(.Negative) == true);
    try expect(cpu.statusHas(.Carry) == false);
    try expect(cpu.statusHas(.Zero) == false);

    // Test carry flag
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x80, ASL, BRK });
    try expect(cpu.a == 0x00);
    try expect(cpu.statusHas(.Carry) == true);
    try expect(cpu.statusHas(.Zero) == true);
    try expect(cpu.statusHas(.Negative) == false);

    // Test zero page
    cpu.memWrite(0x42, 0x55);
    cpu.loadAndRun(&[_]u8{ ASL_ZP, 0x42, BRK });
    try expect(cpu.memRead(0x42) == 0xAA);
    try expect(cpu.statusHas(.Negative) == true);
    try expect(cpu.statusHas(.Carry) == false);
    try expect(cpu.statusHas(.Zero) == false);
}

test "BCC" {
    var cpu = CPU{};

    cpu.loadAndRun(&[_]u8{ BCC, 2, LDA_IMM, 0x42, BRK });
    try expect(cpu.a == 0x00);

    cpu.loadAndRun(&[_]u8{ BCC, 2, NOP, NOP, LDA_IMM, 0x42, BRK });
    try expect(cpu.a == 0x42);
}

test "BCS" {
    var cpu = CPU{};

    cpu.loadAndRun(&[_]u8{ BCS, 2, LDA_IMM, 0x42, BRK });
    try expect(cpu.a == 0x42);

    cpu.loadAndRun(&[_]u8{ SEC, BCS, 2, LDA_IMM, 0x42, BRK });
    try expect(cpu.a == 0x00);

    cpu.loadAndRun(&[_]u8{ SEC, BCS, 2, NOP, NOP, LDA_IMM, 0x42, BRK });
    try expect(cpu.a == 0x42);
}

test "BEQ" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0, BEQ, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0);

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 1, BEQ, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
}

test "BIT" {
    var cpu = CPU{};
    cpu.memWrite(0x42, 0x00);

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0xFF, BIT_ZP, 0x42, BRK });
    try expect(cpu.statusHas(.Zero));
    try expect(!cpu.statusHas(.Negative));
    try expect(!cpu.statusHas(.Overflow));

    // Test both negative and overflow flags
    cpu.memWrite(0x42, 0b1100_0000);
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0xFF, BIT_ZP, 0x42, BRK });
    try expect(!cpu.statusHas(.Zero));
    try expect(cpu.statusHas(.Negative));
    try expect(cpu.statusHas(.Overflow));
}

test "BMI" {
    var cpu = CPU{};

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x80, BMI, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x80);
    try expect(cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x00, BMI, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
    try expect(!cpu.statusHas(.Negative));
}

test "BNE" {
    var cpu = CPU{};

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x01, BNE, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x01);
    try expect(!cpu.statusHas(.Zero));

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x00, BNE, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
    try expect(!cpu.statusHas(.Zero));
}

test "BPL" {
    var cpu = CPU{};

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x80, BPL, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x00, BPL, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x00);
}

test "BVC and BVS" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x50, ADC_IMM, 0x50, BVS, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.statusHas(.Overflow));
    try expect(cpu.a == 0xA0);

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x50, ADC_IMM, 0x50, BVC, 2, LDA_IMM, 0x69, BRK });
    try expect(cpu.statusHas(.Overflow));
    try expect(cpu.a == 0x69);

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x00, ADC_IMM, 0x50, BVS, 2, LDA_IMM, 0x69, BRK });
    try expect(!cpu.statusHas(.Overflow));
    try expect(cpu.a == 0x69);

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x00, ADC_IMM, 0x50, BRK, BVC, 2, LDA_IMM, 0x69, BRK });
    try expect(!cpu.statusHas(.Overflow));
    try expect(cpu.a == 0x50);
}

test "CL*" {
    var cpu = CPU{};
    // CLC
    cpu.loadAndRun(&[_]u8{ SEC, BRK });
    try expect(cpu.statusHas(.Carry));
    cpu.loadAndRun(&[_]u8{ SEC, CLC, BRK });
    try expect(!cpu.statusHas(.Carry));

    // CLD
    cpu.loadAndRun(&[_]u8{ SED, BRK });
    try expect(cpu.statusHas(.DecimalMode));
    cpu.loadAndRun(&[_]u8{ SED, CLD, BRK });
    try expect(!cpu.statusHas(.DecimalMode));

    // CLI
    cpu.loadAndRun(&[_]u8{ SEI, BRK });
    try expect(cpu.statusHas(.InterruptDisable));
    cpu.loadAndRun(&[_]u8{ SEI, CLI, BRK });
    try expect(!cpu.statusHas(.InterruptDisable));

    // CLV
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x50, ADC_IMM, 0x50, BRK });
    try expect(cpu.statusHas(.Overflow));
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x50, ADC_IMM, 0x50, CLV, BRK });
    try expect(!cpu.statusHas(.Overflow));
}

test "CMP*" {
    var cpu = CPU{};
    // CMP (with a)
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x69, CMP_IMM, 0x68, BRK });
    try expect(cpu.statusHas(.Carry));
    try expect(!cpu.statusHas(.Zero));
    try expect(!cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x69, CMP_IMM, 0x69, BRK });
    try expect(cpu.statusHas(.Carry));
    try expect(cpu.statusHas(.Zero));
    try expect(!cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x01, CMP_IMM, 0x02, BRK });
    try expect(!cpu.statusHas(.Carry));
    try expect(!cpu.statusHas(.Zero));
    try expect(cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDX_IMM, 0x69, CPX_IMM, 0x68, BRK });
    try expect(cpu.statusHas(.Carry));
    try expect(!cpu.statusHas(.Zero));
    try expect(!cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDX_IMM, 0x69, CPX_IMM, 0x69, BRK });
    try expect(cpu.statusHas(.Carry));
    try expect(cpu.statusHas(.Zero));
    try expect(!cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDX_IMM, 0x01, CPX_IMM, 0x02, BRK });
    try expect(!cpu.statusHas(.Carry));
    try expect(!cpu.statusHas(.Zero));
    try expect(cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDY_IMM, 0x69, CPY_IMM, 0x68, BRK });
    try expect(cpu.statusHas(.Carry));
    try expect(!cpu.statusHas(.Zero));
    try expect(!cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDY_IMM, 0x69, CPY_IMM, 0x69, BRK });
    try expect(cpu.statusHas(.Carry));
    try expect(cpu.statusHas(.Zero));
    try expect(!cpu.statusHas(.Negative));

    cpu.loadAndRun(&[_]u8{ LDY_IMM, 0x01, CPY_IMM, 0x02, BRK });
    try expect(!cpu.statusHas(.Carry));
    try expect(!cpu.statusHas(.Zero));
    try expect(cpu.statusHas(.Negative));
}

test "DE[C,X,Y]" {
    var cpu = CPU{};
    cpu.memWrite(0x42, 70);
    cpu.loadAndRun(&[_]u8{ DEC_ZP, 0x42, BRK });
    try expect(cpu.memRead(0x42) == 69);

    cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDX_IMM, 70, DEX, BRK });
    try expect(cpu.x == 69);

    try expect(!cpu.statusHas(.Zero));
    cpu.loadAndRun(&[_]u8{ LDX_IMM, 1, DEX, BRK });
    try expect(cpu.x == 0);
    try expect(cpu.statusHas(.Zero));

    cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDY_IMM, 70, DEY, BRK });
    try expect(cpu.y == 69);

    try expect(!cpu.statusHas(.Zero));
    cpu.loadAndRun(&[_]u8{ LDY_IMM, 1, DEY, BRK });
    try expect(cpu.y == 0);
    try expect(cpu.statusHas(.Zero));
}

test "EOR" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0b1000_1111, EOR_IMM, 0b0000_1110, BRK });
    try expect(cpu.statusHas(.Negative));
    try expect(cpu.a == 0b1000_0001);
    try expect(!cpu.statusHas(.Zero));

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0b1000_1111, EOR_IMM, 0b1000_1111, BRK });
    try expect(!cpu.statusHas(.Negative));
    try expect(cpu.statusHas(.Zero));
}

test "JMP" {
    var cpu = CPU{};
    cpu.memWriteU16(0x1234, 0x8005);
    cpu.loadAndRun(&.{ JMP_ABS, 0x34, 0x12, LDX_IMM, 0x42, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
    try expect(cpu.x == 0);

    cpu.memWriteU16(0x1234, 0x8003);
    cpu.loadAndRun(&.{ JMP_ABS, 0x34, 0x12, LDX_IMM, 0x42, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
    try expect(cpu.x == 0x42);
}

test "LDA immediate addressing" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
    try expect(cpu.status & 0b1000_0000 == 0);
    try expect(cpu.status & 0b0000_0010 == 0);
}

test "LDA zero page addressing" {
    var cpu = CPU{};
    cpu.memWrite(0x10, 0x69);
    cpu.loadAndRun(&[_]u8{ LDA_ZP, 0x10, BRK });
    try expect(cpu.a == 0x69);
}

test "LDA zero page X addressing" {
    var cpu = CPU{};
    cpu.memWrite(0x42, 0x69);
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x02, TAX, LDA_ZPX, 0x40, BRK });
    try expect(cpu.a == 0x69);
}

test "LDA absolute addressing" {
    var cpu = CPU{};
    cpu.memWrite(0x1234, 0x69);
    cpu.loadAndRun(&[_]u8{ LDA_ABS, 0x34, 0x12, BRK });
    try expect(cpu.a == 0x69);
}

test "LDA absolute X addressing" {
    var cpu = CPU{};
    cpu.memWrite(0x1234, 0x69);
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x04, TAX, LDA_ABSX, 0x30, 0x12, BRK });
    try expect(cpu.a == 0x69);
}

test "LDX and LDY" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDX_IMM, 0x69, BRK });
    try expect(cpu.x == 0x69);

    cpu.memWrite(0x42, 0x00);
    try expect(!cpu.statusHas(.Zero));
    cpu.loadAndRun(&[_]u8{ LDX_ZP, 0x42, BRK });
    try expect(cpu.x == 0x00);
    try expect(cpu.statusHas(.Zero));

    cpu.loadAndRun(&[_]u8{ LDY_IMM, 0x69, BRK });
    try expect(cpu.y == 0x69);

    cpu.memWrite(0x42, 0x00);
    try expect(!cpu.statusHas(.Zero));
    cpu.loadAndRun(&[_]u8{ LDY_ZP, 0x42, BRK });
    try expect(cpu.y == 0x00);
    try expect(cpu.statusHas(.Zero));
}

test "SE*" {
    // SEC
    var cpu = CPU{};
    try expect(!cpu.statusHas(.Carry));
    cpu.loadAndRun(&[_]u8{ SEC, BRK });
    try expect(cpu.statusHas(.Carry));

    // SED
    cpu = CPU{};
    try expect(!cpu.statusHas(.DecimalMode));
    cpu.loadAndRun(&[_]u8{ SED, BRK });
    try expect(cpu.statusHas(.DecimalMode));

    // SEI
    cpu = CPU{};
    try expect(!cpu.statusHas(.InterruptDisable));
    cpu.loadAndRun(&[_]u8{ SEI, BRK });
    try expect(cpu.statusHas(.InterruptDisable));
}

test "PHA, PLA" {
    var cpu = CPU{};

    cpu.loadAndRun(&.{ LDA_IMM, 0x69, PHA, BRK });
    try expect(cpu.pull() == 0x69);

    cpu.loadAndRun(&.{ LDA_IMM, 0x69, PHA, LDA_IMM, 0x00, BRK });
    try expect(cpu.a == 0x00);

    cpu.loadAndRun(&.{ LDA_IMM, 0x69, PHA, LDA_IMM, 0x00, PLA, BRK });
    try expect(cpu.a == 0x69);
}

test "PHP, PLP" {
    var cpu = CPU{};

    try expect(!cpu.statusHas(.Negative));
    cpu.loadAndRun(&.{ LDA_IMM, 0b1111_1111, PHP, BRK });
    try expect(cpu.statusHas(.Negative));
    try expect(!cpu.statusHas(.Break));
    try expect(!cpu.statusHas(.Break2));

    cpu.loadAndRun(&.{ LDA_IMM, 0b1111_1111, PHP, PLP, BRK });
    try expect(cpu.statusHas(.Negative));
    try expect(!cpu.statusHas(.Break));
    try expect(cpu.statusHas(.Break2));
}

test "STA zero page addressing" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x42, STA_ZP, 0x15, BRK });
    try expect(cpu.memRead(0x15) == 0x42);
}

test "STA absolute addressing" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x42, STA_ABS, 0x34, 0x12, BRK });
    try expect(cpu.memRead(0x1234) == 0x42);
}

test "IN[C,X,Y]" {
    var cpu = CPU{};

    // INC
    cpu.memWrite(0x42, 0x68);
    cpu.loadAndRun(&[_]u8{ INC_ZP, 0x42, BRK });
    try expect(cpu.memRead(0x42) == 0x69);

    // INX
    cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDX_IMM, 0x68, INX, BRK });
    try expect(cpu.x == 0x69);

    // INX overflow
    cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDX_IMM, 0xff, INX, INX, BRK });
    try expect(cpu.x == 1);

    // INY
    cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDY_IMM, 0x68, INY, BRK });
    try expect(cpu.y == 0x69);
}

test "Status flags" {
    var cpu = CPU{};

    try expect(cpu.statusHas(.Carry) == false);
    try expect(cpu.statusHas(.Zero) == false);
    try expect(cpu.statusHas(.InterruptDisable) == false);
    try expect(cpu.statusHas(.DecimalMode) == false);
    try expect(cpu.statusHas(.Break) == false);
    try expect(cpu.statusHas(.Break2) == false);
    try expect(cpu.statusHas(.Overflow) == false);
    try expect(cpu.statusHas(.Negative) == false);

    cpu.statusSet(.Carry);
    cpu.statusSet(.InterruptDisable);
    try expect(cpu.statusHas(.Carry) == true);
    try expect(cpu.statusHas(.Zero) == false);
    try expect(cpu.statusHas(.InterruptDisable) == true);
    try expect(cpu.statusHas(.DecimalMode) == false);
    try expect(cpu.statusHas(.Break) == false);
    try expect(cpu.statusHas(.Break2) == false);
    try expect(cpu.statusHas(.Overflow) == false);
    try expect(cpu.statusHas(.Negative) == false);

    cpu.statusClear(.Carry);
    cpu.statusClear(.InterruptDisable);
    try expect(cpu.statusHas(.Carry) == false);
    try expect(cpu.statusHas(.Zero) == false);
    try expect(cpu.statusHas(.InterruptDisable) == false);
    try expect(cpu.statusHas(.DecimalMode) == false);
    try expect(cpu.statusHas(.Break) == false);
    try expect(cpu.statusHas(.Break2) == false);
    try expect(cpu.statusHas(.Overflow) == false);
    try expect(cpu.statusHas(.Negative) == false);

    cpu.statusSet(.Break);
    cpu.statusSet(.Break2);
    try expect(cpu.statusHas(.Carry) == false);
    try expect(cpu.statusHas(.Zero) == false);
    try expect(cpu.statusHas(.InterruptDisable) == false);
    try expect(cpu.statusHas(.DecimalMode) == false);
    try expect(cpu.statusHas(.Break) == true);
    try expect(cpu.statusHas(.Break2) == true);
    try expect(cpu.statusHas(.Overflow) == false);
    try expect(cpu.statusHas(.Negative) == false);

    // Test zero flag
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x00, BRK });
    try expect(cpu.statusHas(.Zero));
    try expect(cpu.statusHas(.Negative) == false);

    // Test negative flag
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x80, BRK });
    try expect(cpu.statusHas(.Negative));
}
