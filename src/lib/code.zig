const std = @import("std");

pub const Instructions = []const u8;

pub const Opcode = enum(u8) {
    constant,
    add,
    sub,
    mul,
    div,
    pop,
    op_true,
    op_false,
    equal,
    not_equal,
    greater_than,
    jump_not_truthy,
    jump,
    op_null,
    minus,
    bang,
    set_global,
    get_global,
    array,
    hash,
    index,
    call,
    return_value,
    op_return,
    set_local,
    get_local,
    get_builtin,
    closure,
    get_free,
    current_closure,
};

pub const Definition = struct {
    name: []const u8,
    operand_widths: []const u8,
};

const definitions = std.enums.EnumArray(Opcode, Definition).init(.{
    .constant = .{ .name = "OpConstant", .operand_widths = &.{2} },
    .add = .{ .name = "OpAdd", .operand_widths = &.{} },
    .sub = .{ .name = "OpSub", .operand_widths = &.{} },
    .mul = .{ .name = "OpMul", .operand_widths = &.{} },
    .div = .{ .name = "OpDiv", .operand_widths = &.{} },
    .pop = .{ .name = "OpPop", .operand_widths = &.{} },
    .op_true = .{ .name = "OpTrue", .operand_widths = &.{} },
    .op_false = .{ .name = "OpFalse", .operand_widths = &.{} },
    .equal = .{ .name = "OpEqual", .operand_widths = &.{} },
    .not_equal = .{ .name = "OpNotEqual", .operand_widths = &.{} },
    .greater_than = .{ .name = "OpGreaterThan", .operand_widths = &.{} },
    .jump_not_truthy = .{ .name = "OpJumpNotTruthy", .operand_widths = &.{2} },
    .jump = .{ .name = "OpJump", .operand_widths = &.{2} },
    .op_null = .{ .name = "OpNull", .operand_widths = &.{} },
    .minus = .{ .name = "OpMinus", .operand_widths = &.{} },
    .bang = .{ .name = "OpBang", .operand_widths = &.{} },
    .set_global = .{ .name = "OpSetGlobal", .operand_widths = &.{2} },
    .get_global = .{ .name = "OpGetGlobal", .operand_widths = &.{2} },
    .array = .{ .name = "OpArray", .operand_widths = &.{2} },
    .hash = .{ .name = "OpHash", .operand_widths = &.{2} },
    .index = .{ .name = "OpIndex", .operand_widths = &.{} },
    .call = .{ .name = "OpCall", .operand_widths = &.{1} },
    .return_value = .{ .name = "OpReturnValue", .operand_widths = &.{} },
    .op_return = .{ .name = "OpReturn", .operand_widths = &.{} },
    .set_local = .{ .name = "OpSetLocal", .operand_widths = &.{1} },
    .get_local = .{ .name = "OpGetLocal", .operand_widths = &.{1} },
    .get_builtin = .{ .name = "OpGetBuiltin", .operand_widths = &.{1} },
    .closure = .{ .name = "OpClosure", .operand_widths = &.{ 2, 1 } },
    .get_free = .{ .name = "OpGetFree", .operand_widths = &.{1} },
    .current_closure = .{ .name = "OpCurrentClosure", .operand_widths = &.{} },
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
        inline for (1..9) |w| {
            if (width == w) {
                const T = std.meta.Int(.unsigned, w * 8);
                std.mem.writeInt(T, instruction[offset..][0..w], @intCast(operand), .big);
            }
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

pub inline fn readUint16(ins: Instructions) u16 {
    return std.mem.readInt(u16, ins[0..2], .big);
}

pub inline fn readUint8(ins: Instructions) u8 {
    return std.mem.readInt(u8, ins[0..1], .big);
}

pub fn readOperands(def: Definition, ins: Instructions) ReadOperandsResult {
    var result = ReadOperandsResult{};
    var offset: usize = 0;
    for (def.operand_widths) |width| {
        inline for (1..9) |w| {
            if (width == w) {
                const T = std.meta.Int(.unsigned, w * 8);
                result.operands[result.len] = std.mem.readInt(T, ins[offset..][0..w], .big);
                result.len += 1;
            }
        }
        offset += width;
    }
    result.bytes_read = offset;
    return result;
}

pub fn concatInstructions(allocator: std.mem.Allocator, slices: []const []const u8) ![]u8 {
    return std.mem.concat(allocator, u8, slices);
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
