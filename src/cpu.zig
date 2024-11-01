const std = @import("std");
const expect = std.testing.expect;

pub const AddrMode = enum {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    None,
};

pub const OpCode = struct {
    code: u8, // Opcode
    mnemonic: []const u8, // instruction name
    len: u8, // number of bytes
    cycles: u8, // number of cycles
    mode: AddrMode, // address mode
    plus_if_crossed: u8, // + if crossed
};

pub const LDA_IMM = 0xA9;
pub const LDA_ZP = 0xA5;
pub const LDA_ZPX = 0xB5;
pub const LDA_ABS = 0xAD;
pub const LDA_ABSX = 0xBD;
pub const LDA_ABSY = 0xB9;
pub const LDA_INDX = 0xA1;
pub const LDA_INDY = 0xB1;

// OpCode constants for BRK, TAX, INX
pub const BRK = 0x00;
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
    // Load Accumulator (LDA) instructions
    .{ .code = LDA_IMM, .mnemonic = "LDA", .len = 2, .cycles = 2, .mode = .Immediate, .plus_if_crossed = 0 },
    .{ .code = LDA_ZP, .mnemonic = "LDA", .len = 2, .cycles = 3, .mode = .ZeroPage, .plus_if_crossed = 0 },
    .{ .code = LDA_ZPX, .mnemonic = "LDA", .len = 2, .cycles = 4, .mode = .ZeroPageX, .plus_if_crossed = 0 },
    .{ .code = LDA_ABS, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .Absolute, .plus_if_crossed = 0 },
    .{ .code = LDA_ABSX, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteX, .plus_if_crossed = 1 },
    .{ .code = LDA_ABSY, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteY, .plus_if_crossed = 1 },
    .{ .code = LDA_INDX, .mnemonic = "LDA", .len = 2, .cycles = 6, .mode = .IndirectX, .plus_if_crossed = 0 },
    .{ .code = LDA_INDY, .mnemonic = "LDA", .len = 2, .cycles = 5, .mode = .IndirectY, .plus_if_crossed = 1 },

    // Break, Transfer A to X, and Increment X instructions
    .{ .code = BRK, .mnemonic = "BRK", .len = 1, .cycles = 7, .mode = .None, .plus_if_crossed = 0 },
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
        std.mem.copyForwards(u8, self.memory[PROG_ROM_START_ADDR .. PROG_ROM_START_ADDR + program.len], program);
        self.pc = PROG_ROM_START_ADDR;
        self.memWriteU16(ADDR_START, PROG_ROM_START_ADDR);
    }

    pub fn loadAndRun(self: *CPU, program: []const u8) void {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn run(self: *CPU) void {
        while (true) {
            const opcode = self.memRead(self.pc);
            self.pc += 1;

            switch (opcode) {
                0x00 => break,
                LDA_IMM, LDA_ZP, LDA_ZPX, LDA_ABS, LDA_ABSX, LDA_ABSY, LDA_INDX, LDA_INDY => {
                    self.lda(OpcodeMap.get(opcode).?.mode);
                    self.pc += OpcodeMap.get(opcode).?.len - 1;
                },
                TAX => {
                    self.tax();
                    self.pc += OpcodeMap.get(opcode).?.len - 1;
                },
                INX => {
                    self.inx();
                    self.pc += OpcodeMap.get(opcode).?.len - 1;
                },
                else => unreachable,
            }
        }
    }

    fn lda(self: *CPU, mode: AddrMode) void {
        const addr = self.get_op_addr(mode);
        const value = self.memRead(addr);

        self.a = value;
        self.update_zero_and_negative_flag(self.a);
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

test "lda" {
    var cpu = CPU{};
    cpu.memWrite(0x10, 0x42);

    cpu.loadAndRun(&[_]u8{ LDA_ZP, 0x10, 0x00 });
    try expect(cpu.a == 0x42);

    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x69, 0x00 });
    try expect(cpu.a == 0x69);
}

// test "LDA, BRK" {
//     var cpu = CPU{};
//
//     cpu.interpret(&[_]u8{ 0xa9, 0x05, 0x00 });
//
//     try expect(cpu.a == 0x05);
//     try expect(cpu.status & 0b1000_0000 == 0);
//     try expect(cpu.status & 0b0000_0010 == 0);
//
//     cpu.interpret(&[_]u8{ 0xa9, 0x00, 0x00 });
//     try expect(cpu.a == 0x00);
//     try expect(cpu.status & 0b1000_0000 == 0);
//     try expect(cpu.status & 0b0000_0010 == 0b10);
//
//     cpu.interpret(&[_]u8{ 0xa9, 0b1000_1111, 0x00 });
//     try expect(cpu.a == 0b1000_1111);
//     try expect(cpu.status & 0b1000_0000 == 0b1000_0000);
//     try expect(cpu.status & 0b0000_0010 == 0b0);
// }
//
// test "TAX" {
//     var cpu = CPU{};
//     cpu.a = 10;
//     cpu.interpret(&[_]u8{ 0xaa, 0x00 });
//     try expect(cpu.x == 10);
// }
//
// test "INX" {
//     var cpu = CPU{};
//     cpu.x = 0xff;
//     cpu.interpret(&[_]u8{ 0xe8, 0xe8, 0x00 });
//     try expect(cpu.x == 1);
// }
