const std = @import("std");
const testing = std.testing;

const code = @import("./code.zig");
const Opcode = code.Opcode;

test "read operands" {
    const tests = [_]struct {
        op: Opcode,
        operands: []const usize,
    }{
        .{ .op = .constant, .operands = &.{65535} },
        .{ .op = .add, .operands = &.{} },
        .{ .op = .sub, .operands = &.{} },
        .{ .op = .mul, .operands = &.{} },
        .{ .op = .div, .operands = &.{} },
        .{ .op = .op_true, .operands = &.{} },
        .{ .op = .op_false, .operands = &.{} },
        .{ .op = .jump_not_truthy, .operands = &.{65535} },
        .{ .op = .jump, .operands = &.{65535} },
        .{ .op = .op_null, .operands = &.{} },
        .{ .op = .array, .operands = &.{65535} },
        .{ .op = .hash, .operands = &.{65535} },
        .{ .op = .pop, .operands = &.{} },
        .{ .op = .equal, .operands = &.{} },
        .{ .op = .not_equal, .operands = &.{} },
        .{ .op = .greater_than, .operands = &.{} },
        .{ .op = .minus, .operands = &.{} },
        .{ .op = .bang, .operands = &.{} },
        .{ .op = .set_global, .operands = &.{65535} },
        .{ .op = .get_global, .operands = &.{65535} },
        .{ .op = .index, .operands = &.{} },
        .{ .op = .call, .operands = &.{255} },
        .{ .op = .return_value, .operands = &.{} },
        .{ .op = .op_return, .operands = &.{} },
        .{ .op = .set_local, .operands = &.{255} },
        .{ .op = .get_local, .operands = &.{255} },
        .{ .op = .get_builtin, .operands = &.{255} },
        .{ .op = .closure, .operands = &.{ 65535, 255 } },
        .{ .op = .get_free, .operands = &.{255} },
        .{ .op = .current_closure, .operands = &.{} },
    };

    for (tests) |tt| {
        const instruction = try code.make(testing.allocator, tt.op, tt.operands);
        defer testing.allocator.free(instruction);

        const def = code.lookup(tt.op);
        const result = code.readOperands(def, instruction[1..]);

        try testing.expectEqual(tt.operands.len, result.len);
        for (tt.operands, 0..) |expected, i| {
            try testing.expectEqual(expected, result.slice()[i]);
        }
    }
}

test "instructions string" {
    const instructions_list = [_][]const u8{
        try code.make(testing.allocator, .constant, &.{1}),
        try code.make(testing.allocator, .constant, &.{2}),
        try code.make(testing.allocator, .constant, &.{65535}),
        try code.make(testing.allocator, .add, &.{}),
        try code.make(testing.allocator, .pop, &.{}),
        try code.make(testing.allocator, .jump_not_truthy, &.{65535}),
        try code.make(testing.allocator, .jump, &.{65535}),
        try code.make(testing.allocator, .op_null, &.{}),
        try code.make(testing.allocator, .array, &.{65535}),
        try code.make(testing.allocator, .hash, &.{65535}),
        try code.make(testing.allocator, .index, &.{}),
        try code.make(testing.allocator, .sub, &.{}),
        try code.make(testing.allocator, .mul, &.{}),
        try code.make(testing.allocator, .div, &.{}),
        try code.make(testing.allocator, .op_true, &.{}),
        try code.make(testing.allocator, .op_false, &.{}),
        try code.make(testing.allocator, .equal, &.{}),
        try code.make(testing.allocator, .not_equal, &.{}),
        try code.make(testing.allocator, .greater_than, &.{}),
        try code.make(testing.allocator, .minus, &.{}),
        try code.make(testing.allocator, .bang, &.{}),
        try code.make(testing.allocator, .set_global, &.{65535}),
        try code.make(testing.allocator, .get_global, &.{65535}),
        try code.make(testing.allocator, .call, &.{255}),
        try code.make(testing.allocator, .return_value, &.{}),
        try code.make(testing.allocator, .op_return, &.{}),
        try code.make(testing.allocator, .set_local, &.{1}),
        try code.make(testing.allocator, .get_local, &.{1}),
        try code.make(testing.allocator, .get_builtin, &.{1}),
        try code.make(testing.allocator, .closure, &.{ 65535, 255 }),
        try code.make(testing.allocator, .get_free, &.{1}),
        try code.make(testing.allocator, .current_closure, &.{}),
    };
    defer for (instructions_list) |ins| {
        testing.allocator.free(ins);
    };

    const concatted = try code.concatInstructions(testing.allocator, &instructions_list);
    defer testing.allocator.free(concatted);

    const expected =
        \\0000 OpConstant 1
        \\0003 OpConstant 2
        \\0006 OpConstant 65535
        \\0009 OpAdd
        \\0010 OpPop
        \\0011 OpJumpNotTruthy 65535
        \\0014 OpJump 65535
        \\0017 OpNull
        \\0018 OpArray 65535
        \\0021 OpHash 65535
        \\0024 OpIndex
        \\0025 OpSub
        \\0026 OpMul
        \\0027 OpDiv
        \\0028 OpTrue
        \\0029 OpFalse
        \\0030 OpEqual
        \\0031 OpNotEqual
        \\0032 OpGreaterThan
        \\0033 OpMinus
        \\0034 OpBang
        \\0035 OpSetGlobal 65535
        \\0038 OpGetGlobal 65535
        \\0041 OpCall 255
        \\0043 OpReturnValue
        \\0044 OpReturn
        \\0045 OpSetLocal 1
        \\0047 OpGetLocal 1
        \\0049 OpGetBuiltin 1
        \\0051 OpClosure 65535 255
        \\0055 OpGetFree 1
        \\0057 OpCurrentClosure
        \\
    ;

    var output_writer = std.Io.Writer.Allocating.init(testing.allocator);
    defer output_writer.deinit();
    try code.toString(concatted, &output_writer.writer);

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
        .{
            .op = .add,
            .operands = &.{},
            .expected = &.{
                @intFromEnum(Opcode.add),
            },
        },
        .{
            .op = .sub,
            .operands = &.{},
            .expected = &.{
                @intFromEnum(Opcode.sub),
            },
        },
        .{
            .op = .mul,
            .operands = &.{},
            .expected = &.{
                @intFromEnum(Opcode.mul),
            },
        },
        .{
            .op = .div,
            .operands = &.{},
            .expected = &.{
                @intFromEnum(Opcode.div),
            },
        },
        .{
            .op = .pop,
            .operands = &.{},
            .expected = &.{
                @intFromEnum(Opcode.pop),
            },
        },
        .{
            .op = .op_true,
            .operands = &.{},
            .expected = &.{
                @intFromEnum(Opcode.op_true),
            },
        },
        .{
            .op = .op_false,
            .operands = &.{},
            .expected = &.{
                @intFromEnum(Opcode.op_false),
            },
        },
        .{
            .op = .equal,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.equal)},
        },
        .{
            .op = .not_equal,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.not_equal)},
        },
        .{
            .op = .greater_than,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.greater_than)},
        },
        .{
            .op = .jump_not_truthy,
            .operands = &.{65534},
            .expected = &.{ @intFromEnum(Opcode.jump_not_truthy), 255, 254 },
        },
        .{
            .op = .jump,
            .operands = &.{65534},
            .expected = &.{ @intFromEnum(Opcode.jump), 255, 254 },
        },
        .{
            .op = .op_null,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.op_null)},
        },
        .{
            .op = .minus,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.minus)},
        },
        .{
            .op = .bang,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.bang)},
        },
        .{
            .op = .set_global,
            .operands = &.{65534},
            .expected = &.{ @intFromEnum(Opcode.set_global), 255, 254 },
        },
        .{
            .op = .get_global,
            .operands = &.{65534},
            .expected = &.{ @intFromEnum(Opcode.get_global), 255, 254 },
        },
        .{
            .op = .array,
            .operands = &.{65534},
            .expected = &.{ @intFromEnum(Opcode.array), 255, 254 },
        },
        .{
            .op = .hash,
            .operands = &.{65534},
            .expected = &.{ @intFromEnum(Opcode.hash), 255, 254 },
        },
        .{
            .op = .index,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.index)},
        },
        .{
            .op = .call,
            .operands = &.{254},
            .expected = &.{ @intFromEnum(Opcode.call), 254 },
        },
        .{
            .op = .return_value,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.return_value)},
        },
        .{
            .op = .op_return,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.op_return)},
        },
        .{
            .op = .set_local,
            .operands = &.{255},
            .expected = &.{ @intFromEnum(Opcode.set_local), 255 },
        },
        .{
            .op = .get_local,
            .operands = &.{255},
            .expected = &.{ @intFromEnum(Opcode.get_local), 255 },
        },
        .{
            .op = .get_builtin,
            .operands = &.{255},
            .expected = &.{ @intFromEnum(Opcode.get_builtin), 255 },
        },
        .{
            .op = .closure,
            .operands = &.{ 65534, 255 },
            .expected = &.{ @intFromEnum(Opcode.closure), 255, 254, 255 },
        },
        .{
            .op = .get_free,
            .operands = &.{255},
            .expected = &.{ @intFromEnum(Opcode.get_free), 255 },
        },
        .{
            .op = .current_closure,
            .operands = &.{},
            .expected = &.{@intFromEnum(Opcode.current_closure)},
        },
    };

    for (tests) |tt| {
        const instruction = try code.make(testing.allocator, tt.op, tt.operands);
        defer testing.allocator.free(instruction);

        try testing.expectEqual(tt.expected.len, instruction.len);

        for (tt.expected, 0..) |expected_byte, i| {
            try testing.expectEqual(expected_byte, instruction[i]);
        }
    }
}
