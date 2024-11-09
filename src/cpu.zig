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

pub const ASL_IMP = 0x0A;
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

pub const LDA_IMM = 0xA9;
pub const LDA_ZP = 0xA5;
pub const LDA_ZPX = 0xB5;
pub const LDA_ABS = 0xAD;
pub const LDA_ABSX = 0xBD;
pub const LDA_ABSY = 0xB9;
pub const LDA_INDX = 0xA1;
pub const LDA_INDY = 0xB1;

pub const TAX = 0xAA;
pub const INX = 0xE8;

pub const SEC = 0x38;

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

    .{ .code = ASL_IMP, .mnemonic = "ASL", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
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

    .{ .code = INX, .mnemonic = "INX", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = LDA_ABS, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = LDA_ABSX, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteX, .cross_penalty = 1 },
    .{ .code = LDA_ABSY, .mnemonic = "LDA", .len = 3, .cycles = 4, .mode = .AbsoluteY, .cross_penalty = 1 },
    .{ .code = LDA_IMM, .mnemonic = "LDA", .len = 2, .cycles = 2, .mode = .Immediate, .cross_penalty = 0 },
    .{ .code = LDA_INDX, .mnemonic = "LDA", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = LDA_INDY, .mnemonic = "LDA", .len = 2, .cycles = 5, .mode = .IndirectY, .cross_penalty = 1 },
    .{ .code = LDA_ZP, .mnemonic = "LDA", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = LDA_ZPX, .mnemonic = "LDA", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },

    .{ .code = SEC, .mnemonic = "SEC", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },

    .{ .code = STA_ABS, .mnemonic = "STA", .len = 3, .cycles = 4, .mode = .Absolute, .cross_penalty = 0 },
    .{ .code = STA_ABSX, .mnemonic = "STA", .len = 3, .cycles = 5, .mode = .AbsoluteX, .cross_penalty = 0 },
    .{ .code = STA_ABSY, .mnemonic = "STA", .len = 3, .cycles = 5, .mode = .AbsoluteY, .cross_penalty = 0 },
    .{ .code = STA_INDX, .mnemonic = "STA", .len = 2, .cycles = 6, .mode = .IndirectX, .cross_penalty = 0 },
    .{ .code = STA_INDY, .mnemonic = "STA", .len = 2, .cycles = 6, .mode = .IndirectY, .cross_penalty = 0 },
    .{ .code = STA_ZP, .mnemonic = "STA", .len = 2, .cycles = 3, .mode = .ZeroPage, .cross_penalty = 0 },
    .{ .code = STA_ZPX, .mnemonic = "STA", .len = 2, .cycles = 4, .mode = .ZeroPageX, .cross_penalty = 0 },

    .{ .code = TAX, .mnemonic = "TAX", .len = 1, .cycles = 2, .mode = .Implicit, .cross_penalty = 0 },
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

    pub fn run(self: *CPU) void {
        while (true) {
            const opcode = self.memRead(self.pc);
            self.pc += 1;

            const initial_pc = self.pc;

            // std.debug.print("OP = {s} = {any}\n", .{ OpcodeMap.get(opcode).?.mnemonic, OpcodeMap.get(opcode).? });
            switch (opcode) {
                BRK => break,
                NOP => {},
                ADC_IMM, ADC_ZP, ADC_ZPX, ADC_ABS, ADC_ABSX, ADC_ABSY, ADC_INDX, ADC_INDY => {
                    self.adc(OpcodeMap.get(opcode).?.mode);
                },
                AND_IMM, AND_ZP, AND_ZPX, AND_ABS, AND_ABSX, AND_ABSY, AND_INDX, AND_INDY => {
                    self.and_(OpcodeMap.get(opcode).?.mode);
                },
                ASL_IMP, ASL_ZP, ASL_ZPX, ASL_ABS, ASL_ABSX => {
                    _ = self.asl(OpcodeMap.get(opcode).?.mode, opcode == ASL_IMP);
                },
                BCC => self.bcc(),
                BCS => self.bcs(),
                BEQ => self.beq(),
                BIT_ABS, BIT_ZP => self.bit(OpcodeMap.get(opcode).?.mode),
                BMI => self.bmi(),
                BNE => self.bne(),
                BPL => self.bpl(),
                INX => self.inx(),
                LDA_IMM, LDA_ZP, LDA_ZPX, LDA_ABS, LDA_ABSX, LDA_ABSY, LDA_INDX, LDA_INDY => {
                    self.lda(OpcodeMap.get(opcode).?.mode);
                },
                SEC => self.statusSet(.Carry),
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

    fn bcc(self: *CPU) void {
        self.branch(!self.statusHas(.Carry));
    }

    fn bcs(self: *CPU) void {
        self.branch(self.statusHas(.Carry));
    }

    fn beq(self: *CPU) void {
        self.branch(self.statusHas(.Zero));
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

    fn bmi(self: *CPU) void {
        self.branch(self.statusHas(.Negative));
    }

    fn bne(self: *CPU) void {
        self.branch(!self.statusHas(.Zero));
    }

    fn bpl(self: *CPU) void {
        self.branch(!self.statusHas(.Negative));
    }

    fn lda(self: *CPU, mode: AddrMode) void {
        const value = self.memRead(self.get_op_addr(mode));

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
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x55, ASL_IMP, BRK });
    try expect(cpu.a == 0xAA); // 0b0101_0101 -> 0b1010_1010
    try expect(cpu.statusHas(.Negative) == true);
    try expect(cpu.statusHas(.Carry) == false);
    try expect(cpu.statusHas(.Zero) == false);

    // Test carry flag
    cpu.loadAndRun(&[_]u8{ LDA_IMM, 0x80, ASL_IMP, BRK });
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

test "SEC" {
    var cpu = CPU{};
    try expect(cpu.statusHas(.Carry) == false);
    cpu.loadAndRun(&[_]u8{ SEC, BRK });
    try expect(cpu.statusHas(.Carry) == true);
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
