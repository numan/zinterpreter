const std = @import("std");
const testing = std.testing;

const compiler = @import("./compiler.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;
const Object = @import("./object.zig").Object;
const Vm = @import("./vm.zig").Vm;

const Compiler = compiler.Compiler;

const Expected = union(enum) {
    int: i64,
    boolean: bool,
    null,
};

const VmTestCase = struct {
    input: []const u8,
    expected: Expected,
};

fn parse(allocator: std.mem.Allocator, input: []const u8) !*@import("./ast.zig").Program {
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    return try parser.parse();
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

fn testBooleanObject(expected: bool, obj: Object) !void {
    const boolean = switch (obj) {
        .bool => |value| value,
        else => {
            std.debug.print("object is not Boolean. got={s}\n", .{obj.typeName()});
            return error.TestUnexpectedResult;
        },
    };
    try testing.expectEqual(expected, boolean.value);
}

fn testNullObject(obj: Object) !void {
    if (obj != .null) {
        std.debug.print("object is not Null. got={s}\n", .{obj.typeName()});
        return error.TestUnexpectedResult;
    }
}

fn testExpectedObject(expected: Expected, obj: Object) !void {
    switch (expected) {
        .int => |val| try testIntegerObject(val, obj),
        .boolean => |val| try testBooleanObject(val, obj),
        .null => try testNullObject(obj),
    }
}

fn runVmTests(tests: []const VmTestCase) !void {
    const allocator = testing.allocator;

    for (tests) |tt| {
        var parser = blk: {
            var lexer = Lexer.init(tt.input);
            break :blk Parser.init(allocator, &lexer);
        };
        defer parser.deinit();
        const program = try parser.parse();

        var comp = Compiler.init(allocator);
        defer comp.deinit();
        try comp.compile(program);

        var vm = Vm.init(comp.bytecode());
        try vm.run();

        const stack_elem = vm.lastPoppedStackElem() orelse {
            std.debug.print("stackTop returned null\n", .{});
            return error.TestUnexpectedResult;
        };

        try testExpectedObject(tt.expected, stack_elem);
    }
}

test "integer arithmetic" {
    const tests = [_]VmTestCase{
        .{ .input = "1", .expected = .{ .int = 1 } },
        .{ .input = "2", .expected = .{ .int = 2 } },
        .{ .input = "1 + 2", .expected = .{ .int = 3 } },
        .{ .input = "1 - 2", .expected = .{ .int = -1 } },
        .{ .input = "1 * 2", .expected = .{ .int = 2 } },
        .{ .input = "4 / 2", .expected = .{ .int = 2 } },
        .{ .input = "50 / 2 * 2 + 10 - 5", .expected = .{ .int = 55 } },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = .{ .int = 10 } },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = .{ .int = 32 } },
        .{ .input = "5 * 2 + 10", .expected = .{ .int = 20 } },
        .{ .input = "5 + 2 * 10", .expected = .{ .int = 25 } },
        .{ .input = "5 * (2 + 10)", .expected = .{ .int = 60 } },
    };

    try runVmTests(&tests);
}

test "boolean expressions" {
    const tests = [_]VmTestCase{
        .{ .input = "true", .expected = .{ .boolean = true } },
        .{ .input = "false", .expected = .{ .boolean = false } },
        .{ .input = "1 < 2", .expected = .{ .boolean = true } },
        .{ .input = "1 > 2", .expected = .{ .boolean = false } },
        .{ .input = "1 < 1", .expected = .{ .boolean = false } },
        .{ .input = "1 > 1", .expected = .{ .boolean = false } },
        .{ .input = "1 == 1", .expected = .{ .boolean = true } },
        .{ .input = "1 != 1", .expected = .{ .boolean = false } },
        .{ .input = "1 == 2", .expected = .{ .boolean = false } },
        .{ .input = "1 != 2", .expected = .{ .boolean = true } },
        .{ .input = "true == true", .expected = .{ .boolean = true } },
        .{ .input = "false == false", .expected = .{ .boolean = true } },
        .{ .input = "true == false", .expected = .{ .boolean = false } },
        .{ .input = "true != false", .expected = .{ .boolean = true } },
        .{ .input = "false != true", .expected = .{ .boolean = true } },
        .{ .input = "(1 < 2) == true", .expected = .{ .boolean = true } },
        .{ .input = "(1 < 2) == false", .expected = .{ .boolean = false } },
        .{ .input = "(1 > 2) == true", .expected = .{ .boolean = false } },
        .{ .input = "(1 > 2) == false", .expected = .{ .boolean = true } },
    };

    try runVmTests(&tests);
}

test "prefix expressions" {
    const tests = [_]VmTestCase{
        .{ .input = "-5", .expected = .{ .int = -5 } },
        .{ .input = "-10", .expected = .{ .int = -10 } },
        .{ .input = "-50 + 100 + -50", .expected = .{ .int = 0 } },
        .{ .input = "!true", .expected = .{ .boolean = false } },
        .{ .input = "!false", .expected = .{ .boolean = true } },
        .{ .input = "!5", .expected = .{ .boolean = false } },
        .{ .input = "!!true", .expected = .{ .boolean = true } },
        .{ .input = "!!false", .expected = .{ .boolean = false } },
        .{ .input = "!!5", .expected = .{ .boolean = true } },
    };

    try runVmTests(&tests);
}

test "conditionals" {
    const tests = [_]VmTestCase{
        .{ .input = "if (true) { 10 }", .expected = .{ .int = 10 } },
        .{ .input = "if (true) { 10 } else { 20 }", .expected = .{ .int = 10 } },
        .{ .input = "if (false) { 10 } else { 20 }", .expected = .{ .int = 20 } },
        .{ .input = "if (1) { 10 }", .expected = .{ .int = 10 } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .int = 10 } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .int = 10 } },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .int = 20 } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .null },
        .{ .input = "if (false) { 10 }", .expected = .null },
        .{ .input = "if ((if (false) { 10 })) { 10 } else { 20 }", .expected = .{ .int = 20 } },
    };

    try runVmTests(&tests);
}

test "let statements" {
    const tests = [_]VmTestCase{
        .{ .input = "let one = 1; one;", .expected = .{ .int = 1 } },
        .{ .input = "let one = 1; let two = 2; one + two;", .expected = .{ .int = 3 } },
        .{ .input = "let one = 1; let two = one; two;", .expected = .{ .int = 1 } },
        .{ .input = "let x = 5; x;", .expected = .{ .int = 5 } },
        .{ .input = "let x = 5; let y = 10; x + y;", .expected = .{ .int = 15 } },
    };

    try runVmTests(&tests);
}
