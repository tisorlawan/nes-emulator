const std = @import("std");
const expect = std.testing.expect;

pub const AddrMode = enum {
    /// Immediate addressing uses the next byte as the operand value directly
    /// Example: LDA #$05 - Load the value $05 into accumulator
    Immediate,

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
    plus_if_crossed: u8, // + if crossed
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

pub const LDA_IMM = 0xA9;
pub const LDA_ZP = 0xA5;
pub const LDA_ZPX = 0xB5;
pub const LDA_ABS = 0xAD;
pub const LDA_ABSX = 0xBD;
pub const LDA_ABSY = 0xB9;
pub const LDA_INDX = 0xA1;
pub const LDA_INDY = 0xB1;

// OpCode constants for BRK, TAX, INX
pub const TAX = 0xAA;
pub const INX = 0xE8;

// OpCode constants for STA
pub const STA_ZP = 0x85;
pub const STA_ZPX = 0x95;
pub const STA_ABS = 0x8D;
pub const STA_ABSX = 0x9D;
pub const STA_ABSY = 0x99;
pub const STA_INDX = 0x81;
pub const STA_INDY = 0x91;

const opCodes = [_]OpCode{
    .{ .code = BRK, .mnemonic = "BRK", .len = 1, .cycles = 7, .mode = .None, .plus_if_crossed = 0 },
    .{ .code = NOP, .mnemonic = "NOP", .len = 1, .cycles = 2, .mode = .None, .plus_if_crossed = 0 },

    .{ .code = ADC_IMM, .mnemonic = "ADC", .len = 2, .cycles = 2, .mode = .Immediate, .plus_if_crossed = 0 },
    .{ .code = ADC_ZP, .mnemonic = "ADC", .len = 2, .cycles = 3, .mode = .ZeroPage, .plus_if_crossed = 0 },
    .{ .code = ADC_ZPX, .mnemonic = "ADC", .len = 2, .cycles = 4, .mode = .ZeroPageX, .plus_if_crossed = 0 },
    .{ .code = ADC_ABS, .mnemonic = "ADC", .len = 3, .cycles = 4, .mode = .Absolute, .plus_if_crossed = 0 },
    .{ .code = ADC_ABSX, .mnemonic = "ADC", .len = 3, .cycles = 4, .mode = .AbsoluteX, .plus_if_crossed = 1 },
    .{ .code = ADC_ABSY, .mnemonic = "ADC", .len = 3, .cycles = 4, .mode = .AbsoluteY, .plus_if_crossed = 1 },
    .{ .code = ADC_INDX, .mnemonic = "ADC", .len = 2, .cycles = 6, .mode = .IndirectX, .plus_if_crossed = 0 },
    .{ .code = ADC_INDY, .mnemonic = "ADC", .len = 2, .cycles = 5, .mode = .IndirectY, .plus_if_crossed = 1 },

    // Load Accumulator (LDA) instructions
    .{ .code = LDA_IMM, .mnemonic = "LDA", .len = 2, .cycles = 2, .mode = .Immediate, .plus_if_crossed = 0 },
    .{ .code = LDA_ZP, .mnemonic = "LDA", .len = 2, .cycles = 3, .mode = .ZeroPage, .plus_if_crossed = 0 },
    .{ .code = LDA_ZPX, .mnemonic = "LDA", .len = 2, .cycles = 4, .mode = .ZeroPageX, .plus_if_crossed = 0 },
    .{ .code = LDA_ABS, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .Absolute, .plus_if_crossed = 0 },
    .{ .code = LDA_ABSX, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteX, .plus_if_crossed = 1 },
    .{ .code = LDA_ABSY, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteY, .plus_if_crossed = 1 },
    .{ .code = LDA_INDX, .mnemonic = "LDA", .len = 2, .cycles = 6, .mode = .IndirectX, .plus_if_crossed = 0 },
    .{ .code = LDA_INDY, .mnemonic = "LDA", .len = 2, .cycles = 5, .mode = .IndirectY, .plus_if_crossed = 1 },

    // Transfer A to X, and Increment X instructions
    .{ .code = TAX, .mnemonic = "TAX", .len = 1, .cycles = 2, .mode = .None, .plus_if_crossed = 0 },
    .{ .code = INX, .mnemonic = "INX", .len = 1, .cycles = 2, .mode = .None, .plus_if_crossed = 0 },

    // Store Accumulator (STA) instructions
    .{ .code = STA_ZP, .mnemonic = "STA", .len = 2, .cycles = 3, .mode = .ZeroPage, .plus_if_crossed = 0 },
    .{ .code = STA_ZPX, .mnemonic = "STA", .len = 2, .cycles = 4, .mode = .ZeroPageX, .plus_if_crossed = 0 },
    .{ .code = STA_ABS, .mnemonic = "STA", .len = 3, .cycles = 4, .mode = .Absolute, .plus_if_crossed = 0 },
    .{ .code = STA_ABSX, .mnemonic = "STA", .len = 3, .cycles = 5, .mode = .AbsoluteX, .plus_if_crossed = 0 },
    .{ .code = STA_ABSY, .mnemonic = "STA", .len = 3, .cycles = 5, .mode = .AbsoluteY, .plus_if_crossed = 0 },
    .{ .code = STA_INDX, .mnemonic = "STA", .len = 2, .cycles = 6, .mode = .IndirectX, .plus_if_crossed = 0 },
    .{ .code = STA_INDY, .mnemonic = "STA", .len = 2, .cycles = 6, .mode = .IndirectY, .plus_if_crossed = 0 },
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
    a: u8 = 0,
    x: u8 = 0,
    y: u8 = 0,
    status: u8 = 0,
    pc: u16 = 0,
    memory: [0xFFFF]u8 = .{0} ** 0xFFFF,

    const PROG_ROM_START_ADDR = 0x8000;
    const ADDR_START = 0xFFFC;

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

    pub fn statusUnset(self: *CPU, status: Status) void {
        self.status &= (~@intFromEnum(status));
    }

    pub fn statusIs(self: *CPU, status: Status) bool {
        return (self.status & @intFromEnum(status)) != 0;
    }

    pub fn run(self: *CPU) void {
        while (true) {
            const opcode = self.memRead(self.pc);
            self.pc += 1;

            const initial_pc = self.pc;

            // std.debug.print("OP = {s} = {any}\n", .{ OpcodeMap.get(opcode).?.mnemonic, OpcodeMap.get(opcode).? });
            switch (opcode) {
                BRK => break,
                NOP => {},
                // ADC_IMM, ADC_ZP, ADC_ZPX, ADC_ABS, ADC_ABSX, ADC_ABSY, ADC_INDX, ADC_INDY => {
                //     self.adc(OpcodeMap.get(opcode).?.mode);
                // },
                LDA_IMM, LDA_ZP, LDA_ZPX, LDA_ABS, LDA_ABSX, LDA_ABSY, LDA_INDX, LDA_INDY => {
                    self.lda(OpcodeMap.get(opcode).?.mode);
                },
                STA_ZP, STA_ZPX, STA_ABS, STA_ABSX, STA_ABSY, STA_INDX, STA_INDY => {
                    self.sta(OpcodeMap.get(opcode).?.mode);
                },
                TAX => self.tax(),
                INX => self.inx(),
                else => unreachable,
            }

            if (initial_pc == self.pc) {
                self.pc += OpcodeMap.get(opcode).?.len - 1;
            }
        }
    }

    // fn adc(self: *CPU, mode: AddrMode) void {
    //     const addr = self.get_op_addr(mode);
    //     const value = self.memRead(addr);
    // }

    fn lda(self: *CPU, mode: AddrMode) void {
        const addr = self.get_op_addr(mode);
        const value = self.memRead(addr);

        self.a = value;
        self.update_zero_and_negative_flag(self.a);
    }

    fn sta(self: *CPU, mode: AddrMode) void {
        const addr = self.get_op_addr(mode);
        self.memWrite(addr, self.a);
    }

    fn tax(self: *CPU) void {
        self.x = self.a;
        self.update_zero_and_negative_flag(self.x);
    }

    fn inx(self: *CPU) void {
        self.x +%= 1;
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
            AddrMode.None => unreachable,
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
};

test "NOP" {
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ NOP, NOP, LDA_IMM, 0x69, BRK });
    try expect(cpu.a == 0x69);
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
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x02, TAX, LDA_ZPX, 0x40, BRK }); // 0x40 + 0x02 = 0x42
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

test "Register instructions" {
    // Test TAX
    var cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x69, TAX, BRK });
    try expect(cpu.x == 0x69);

    // Test INX
    cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x68, TAX, INX, BRK });
    try expect(cpu.x == 0x69);

    // Test INX overflow
    cpu = CPU{};
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0xff, TAX, INX, INX, BRK });
    try expect(cpu.x == 1);
}

test "Status flags" {
    var cpu = CPU{};

    try expect(cpu.statusIs(.Carry) == false);
    try expect(cpu.statusIs(.Zero) == false);
    try expect(cpu.statusIs(.InterruptDisable) == false);
    try expect(cpu.statusIs(.DecimalMode) == false);
    try expect(cpu.statusIs(.Break) == false);
    try expect(cpu.statusIs(.Break2) == false);
    try expect(cpu.statusIs(.Overflow) == false);
    try expect(cpu.statusIs(.Negative) == false);

    cpu.statusSet(.Carry);
    cpu.statusSet(.InterruptDisable);
    try expect(cpu.statusIs(.Carry) == true);
    try expect(cpu.statusIs(.Zero) == false);
    try expect(cpu.statusIs(.InterruptDisable) == true);
    try expect(cpu.statusIs(.DecimalMode) == false);
    try expect(cpu.statusIs(.Break) == false);
    try expect(cpu.statusIs(.Break2) == false);
    try expect(cpu.statusIs(.Overflow) == false);
    try expect(cpu.statusIs(.Negative) == false);

    cpu.statusUnset(.Carry);
    cpu.statusUnset(.InterruptDisable);
    try expect(cpu.statusIs(.Carry) == false);
    try expect(cpu.statusIs(.Zero) == false);
    try expect(cpu.statusIs(.InterruptDisable) == false);
    try expect(cpu.statusIs(.DecimalMode) == false);
    try expect(cpu.statusIs(.Break) == false);
    try expect(cpu.statusIs(.Break2) == false);
    try expect(cpu.statusIs(.Overflow) == false);
    try expect(cpu.statusIs(.Negative) == false);

    cpu.statusSet(.Break);
    cpu.statusSet(.Break2);
    try expect(cpu.statusIs(.Carry) == false);
    try expect(cpu.statusIs(.Zero) == false);
    try expect(cpu.statusIs(.InterruptDisable) == false);
    try expect(cpu.statusIs(.DecimalMode) == false);
    try expect(cpu.statusIs(.Break) == true);
    try expect(cpu.statusIs(.Break2) == true);
    try expect(cpu.statusIs(.Overflow) == false);
    try expect(cpu.statusIs(.Negative) == false);

    // Test zero flag
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x00, BRK });
    try expect(cpu.statusIs(.Zero));
    try expect(cpu.statusIs(.Negative) == false);

    // Test negative flag
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x80, BRK });
    try expect(cpu.statusIs(.Negative));
}
