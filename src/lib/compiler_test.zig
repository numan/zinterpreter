const std = @import("std");
const testing = std.testing;

const code = @import("./code.zig");
const compiler = @import("./compiler.zig");
const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;
const Object = @import("./object.zig").Object;

const Compiler = compiler.Compiler;

const CompilerTestCase = struct {
    input: []const u8,
    expected_constants: []const i64,
    expected_instructions: []const []const u8,
};

fn testInstructions(allocator: std.mem.Allocator, expected: []const []const u8, actual: code.Instructions) !void {
    const concatted = try code.concatInstructions(allocator, expected);
    defer allocator.free(concatted);

    if (!std.mem.eql(u8, concatted, actual)) {
        var expected_writer = std.Io.Writer.Allocating.init(allocator);
        defer expected_writer.deinit();
        try code.toString(concatted, &expected_writer.writer);

        var actual_writer = std.Io.Writer.Allocating.init(allocator);
        defer actual_writer.deinit();
        try code.toString(actual, &actual_writer.writer);

        std.debug.print("wrong instructions.\nwant:\n{s}got:\n{s}\n", .{
            expected_writer.written(),
            actual_writer.written(),
        });
        return error.TestUnexpectedResult;
    }
}

fn testConstants(expected: []const i64, actual: []const Object) !void {
    try testing.expectEqual(expected.len, actual.len);

    for (expected, 0..) |constant, i| {
        try testIntegerObject(constant, actual[i]);
    }
}

fn testIntegerObject(expected: i64, obj: Object) !void {
    const integer = switch (obj) {
        .int => |value| value,
        else => {
            std.debug.print("object is not Integer. got={s}\n", .{obj.typeName()});
            return error.TestUnexpectedResult;
        },
    };

    try testing.expectEqual(expected, integer.value);
}

fn runCompilerTests(allocator: std.mem.Allocator, tests: []const CompilerTestCase) !void {
    for (tests) |tt| {
        var lexer = Lexer.init(tt.input);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();
        const program = try parser.parse();

        var comp = Compiler.init(allocator);
        defer comp.deinit();
        try comp.compile(program);

        const bytecode = comp.bytecode();

        try testInstructions(allocator, tt.expected_instructions, bytecode.instructions);
        try testConstants(tt.expected_constants, bytecode.constants);
    }
}

test "integer arithmetic" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_add = try code.make(allocator, .add, &.{});
    defer allocator.free(op_add);

    const tests = [_]CompilerTestCase{
        .{
            .input = "1 + 2",
            .expected_constants = &.{ 1, 2 },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_add,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}
