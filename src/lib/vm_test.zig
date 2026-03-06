const std = @import("std");
const testing = std.testing;

const compiler = @import("./compiler.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;
const Object = @import("./object.zig").Object;
const Vm = @import("./vm.zig").Vm;

const Compiler = compiler.Compiler;

const VmTestCase = struct {
    input: []const u8,
    expected: i64,
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

fn testExpectedObject(expected: i64, obj: Object) !void {
    try testIntegerObject(expected, obj);
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

        const stack_elem = vm.stackTop() orelse {
            std.debug.print("stackTop returned null\n", .{});
            return error.TestUnexpectedResult;
        };

        try testExpectedObject(tt.expected, stack_elem);
    }
}

test "integer arithmetic" {
    const tests = [_]VmTestCase{
        .{ .input = "1", .expected = 1 },
        .{ .input = "2", .expected = 2 },
        .{ .input = "1 + 2", .expected = 3 },
    };

    try runVmTests(&tests);
}
