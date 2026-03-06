const std = @import("std");
const testing = std.testing;

pub const Instructions = []const u8;

pub const Opcode = enum(u8) {
    constant,
};

pub const Definition = struct {
    name: []const u8,
    operand_widths: []const u8,
};

const definitions = std.enums.EnumArray(Opcode, Definition).init(.{
    .constant = .{ .name = "OpConstant", .operand_widths = &.{2} },
});

pub fn lookup(op: Opcode) Definition {
    return definitions.get(op);
}

pub fn make(allocator: std.mem.Allocator, op: Opcode, operands: []const usize) ![]u8 {
    const def = lookup(op);

    var instruction_len: usize = 1; // 1 byte for the opcode itself
    for (def.operand_widths) |w| {
        instruction_len += w;
    }

    var instruction = try allocator.alloc(u8, instruction_len);
    instruction[0] = @intFromEnum(op);

    var offset: usize = 1;
    for (operands, 0..) |operand, i| {
        const width = def.operand_widths[i];
        switch (width) {
            2 => {
                const val: u16 = @intCast(operand);
                std.mem.writeInt(u16, instruction[offset..][0..2], val, .big);
            },
            else => {},
        }
        offset += width;
    }

    return instruction;
}

pub const MaxOperands = blk: {
    var max_operands: usize = 0;
    for (std.meta.fields(Opcode)) |field| {
        const op = @field(Opcode, field.name);
        const operand_count = definitions.get(op).operand_widths.len;
        if (operand_count > max_operands) {
            max_operands = operand_count;
        }
    }
    break :blk max_operands;
};

pub const ReadOperandsResult = struct {
    operands: [MaxOperands]usize = undefined,
    len: usize = 0,
    bytes_read: usize = 0,

    pub fn slice(self: *const @This()) []const usize {
        return self.operands[0..self.len];
    }
};

pub fn readUint16(ins: Instructions) u16 {
    return std.mem.readInt(u16, ins[0..2], .big);
}

pub fn readOperands(def: Definition, ins: Instructions) ReadOperandsResult {
    var result = ReadOperandsResult{};
    var offset: usize = 0;
    for (def.operand_widths) |width| {
        switch (width) {
            2 => {
                result.operands[result.len] = readUint16(ins[offset..]);
                result.len += 1;
            },
            else => {},
        }
        offset += width;
    }
    result.bytes_read = offset;
    return result;
}

pub fn concatInstructions(allocator: std.mem.Allocator, slices: []const []const u8) ![]u8 {
    var total_len: usize = 0;
    for (slices) |ins| {
        total_len += ins.len;
    }
    var buf = try allocator.alloc(u8, total_len);
    var offset: usize = 0;
    for (slices) |ins| {
        @memcpy(buf[offset..][0..ins.len], ins);
        offset += ins.len;
    }
    return buf;
}

pub fn toString(instructions: Instructions, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    var i: usize = 0;
    while (i < instructions.len) {
        const op: Opcode = @enumFromInt(instructions[i]);
        const def = lookup(op);
        const result = readOperands(def, instructions[i + 1 ..]);
        try writer.print("{d:0>4} {s}", .{ i, def.name });
        for (result.slice()) |operand| {
            try writer.print(" {d}", .{operand});
        }
        try writer.print("\n", .{});
        i += 1 + result.bytes_read;
    }
}

test "read operands" {
    const tests = [_]struct {
        op: Opcode,
        operands: []const usize,
    }{
        .{ .op = .constant, .operands = &.{65535} },
    };

    for (tests) |tt| {
        const instruction = try make(testing.allocator, tt.op, tt.operands);
        defer testing.allocator.free(instruction);

        const def = lookup(tt.op);
        const result = readOperands(def, instruction[1..]);

        try testing.expectEqual(tt.operands.len, result.len);
        for (tt.operands, 0..) |expected, i| {
            try testing.expectEqual(expected, result.slice()[i]);
        }
    }
}

test "instructions string" {
    const instructions_list = [_][]const u8{
        try make(testing.allocator, .constant, &.{1}),
        try make(testing.allocator, .constant, &.{2}),
        try make(testing.allocator, .constant, &.{65535}),
    };
    defer for (instructions_list) |ins| {
        testing.allocator.free(ins);
    };

    const concatted = try concatInstructions(testing.allocator, &instructions_list);
    defer testing.allocator.free(concatted);

    const expected =
        \\0000 OpConstant 1
        \\0003 OpConstant 2
        \\0006 OpConstant 65535
        \\
    ;

    var output_writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer output_writer.deinit();
    try toString(concatted, &output_writer.writer);

    try testing.expectEqualStrings(expected, output_writer.written());
}

test "make" {
    const tests = [_]struct {
        op: Opcode,
        operands: []const usize,
        expected: []const u8,
    }{
        .{
            .op = .constant,
            .operands = &.{65534},
            .expected = &.{ @intFromEnum(Opcode.constant), 255, 254 },
        },
    };

    for (tests) |tt| {
        const instruction = try make(testing.allocator, tt.op, tt.operands);
        defer testing.allocator.free(instruction);

        try testing.expectEqual(tt.expected.len, instruction.len);

        for (tt.expected, 0..) |expected_byte, i| {
            try testing.expectEqual(expected_byte, instruction[i]);
        }
    }
}
