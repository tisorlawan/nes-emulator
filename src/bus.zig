const std = @import("std");
pub const Bus = struct {
    cpu_vram: [2048]u8 = .{0} ** 2048, // 11 bits

    const RAM: u16 = 0x0000;
    const RAM_MIRRORS_END: u16 = 0x1FFF;
    const PPU_REGISTERS: u16 = 0x2000;
    const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

    pub fn memRead(self: *Bus, addr: u16) u8 {
        return switch (addr) {
            RAM...RAM_MIRRORS_END => blk: {
                const mirror_down_addr = 0b00000111_11111111 & addr;
                break :blk self.cpu_vram[@as(usize, @intCast(mirror_down_addr))];
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRRORS_END => {
                const _mirror_down_addr = addr & 0b00100000_00000111;
                _ = _mirror_down_addr;
                unreachable;
            },
            else => blk: {
                std.debug.print("Ignoring mem access at 0x{X}\n", .{addr});
                break :blk 0;
            },
        };
    }

    pub fn memReadU16(self: *Bus, addr: u16) u16 {
        const lo = self.memRead(addr);
        const hi = self.memRead(addr +% 1);
        return (@as(u16, hi) << 8) | lo;
    }

    pub fn memWrite(self: *Bus, addr: u16, data: u8) void {
        switch (addr) {
            RAM...RAM_MIRRORS_END => {
                const mirror_down_addr = 0b00000111_11111111 & addr;
                self.cpu_vram[@as(usize, @intCast(mirror_down_addr))] = data;
            },
            PPU_REGISTERS...PPU_REGISTERS_MIRRORS_END => {
                const _mirror_down_addr = addr & 0b00100000_00000111;
                _ = _mirror_down_addr;
                unreachable;
            },
            else => {
                std.debug.print("Ignoring mem write at 0x{X}\n", .{addr});
            },
        }
    }

    pub fn memWriteU16(self: *Bus, addr: u16, value: u16) void {
        const hi: u8 = @truncate(value >> 8);
        const lo: u8 = @truncate(value & 0x00FF);
        self.memWrite(addr, lo);
        self.memWrite(addr +% 1, hi);
    }
};
