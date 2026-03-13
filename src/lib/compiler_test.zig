const std = @import("std");
const testing = std.testing;

const code = @import("./code.zig");
const compiler = @import("./compiler.zig");
const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;
const Object = @import("./object.zig").Object;
const SymbolTable = @import("./symbol_table.zig").SymbolTable;

const Compiler = compiler.Compiler;

const ExpectedConstant = union(enum) {
    int: i64,
    string: []const u8,
    instructions: []const code.Instructions,
};

const CompilerTestCase = struct {
    input: []const u8,
    expected_constants: []const ExpectedConstant,
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

fn testConstants(allocator: std.mem.Allocator, expected: []const ExpectedConstant, actual: []const Object) !void {
    try testing.expectEqual(expected.len, actual.len);

    for (expected, 0..) |constant, i| {
        switch (constant) {
            .int => |exp| try testIntegerObject(exp, actual[i]),
            .string => |exp| try testStringObject(exp, actual[i]),
            .instructions => |exp| {
                const fn_obj = switch (actual[i]) {
                    .compiled_function => |f| f,
                    else => return error.TestUnexpectedResult,
                };
                try testInstructions(allocator, exp, fn_obj.instructions);
            },
        }
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

fn testStringObject(expected: []const u8, obj: Object) !void {
    const string = switch (obj) {
        .string => |value| value,
        else => {
            std.debug.print("object is not String. got={s}\n", .{obj.typeName()});
            return error.TestUnexpectedResult;
        },
    };

    try testing.expectEqualStrings(expected, string.value);
}

fn runCompilerTests(allocator: std.mem.Allocator, tests: []const CompilerTestCase) !void {
    for (tests) |tt| {
        var lexer = Lexer.init(tt.input);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();
        const program = try parser.parse();

        var symbol_table = SymbolTable.init(allocator);
        defer symbol_table.deinit();
        var constants = std.ArrayList(Object).empty;
        defer constants.deinit(allocator);
        var comp = Compiler.init(allocator, &symbol_table, &constants, allocator);
        defer comp.deinit();
        try comp.enterScope();
        try comp.compile(program);

        const bytecode = comp.bytecode();

        try testInstructions(allocator, tt.expected_instructions, bytecode.instructions);
        try testConstants(allocator, tt.expected_constants, bytecode.constants);
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
    const op_sub = try code.make(allocator, .sub, &.{});
    defer allocator.free(op_sub);
    const op_mul = try code.make(allocator, .mul, &.{});
    defer allocator.free(op_mul);
    const op_div = try code.make(allocator, .div, &.{});
    defer allocator.free(op_div);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);

    const tests = [_]CompilerTestCase{
        .{
            .input = "1 + 2",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_add,
                op_pop,
            },
        },
        .{
            .input = "1 - 2",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_sub,
                op_pop,
            },
        },
        .{
            .input = "1 * 2",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_mul,
                op_pop,
            },
        },
        .{
            .input = "6 / 3",
            .expected_constants = &.{ .{ .int = 6 }, .{ .int = 3 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_div,
                op_pop,
            },
        },
        .{
            .input = "1; 2",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_pop,
                op_constant_1,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "boolean expressions" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_true = try code.make(allocator, .op_true, &.{});
    defer allocator.free(op_true);
    const op_false = try code.make(allocator, .op_false, &.{});
    defer allocator.free(op_false);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);
    const op_equal = try code.make(allocator, .equal, &.{});
    defer allocator.free(op_equal);
    const op_not_equal = try code.make(allocator, .not_equal, &.{});
    defer allocator.free(op_not_equal);
    const op_greater_than = try code.make(allocator, .greater_than, &.{});
    defer allocator.free(op_greater_than);

    const tests = [_]CompilerTestCase{
        .{
            .input = "true",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_true,
                op_pop,
            },
        },
        .{
            .input = "false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_false,
                op_pop,
            },
        },
        .{
            .input = "true; false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_true,
                op_pop,
                op_false,
                op_pop,
            },
        },
        .{
            .input = "1 > 2",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_greater_than,
                op_pop,
            },
        },
        .{
            .input = "1 < 2",
            .expected_constants = &.{ .{ .int = 2 }, .{ .int = 1 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_greater_than,
                op_pop,
            },
        },
        .{
            .input = "1 == 2",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_equal,
                op_pop,
            },
        },
        .{
            .input = "1 != 2",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_not_equal,
                op_pop,
            },
        },
        .{
            .input = "true == false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_true,
                op_false,
                op_equal,
                op_pop,
            },
        },
        .{
            .input = "true != false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_true,
                op_false,
                op_not_equal,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "prefix expressions" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_true = try code.make(allocator, .op_true, &.{});
    defer allocator.free(op_true);
    const op_false = try code.make(allocator, .op_false, &.{});
    defer allocator.free(op_false);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);
    const op_minus = try code.make(allocator, .minus, &.{});
    defer allocator.free(op_minus);
    const op_bang = try code.make(allocator, .bang, &.{});
    defer allocator.free(op_bang);

    const tests = [_]CompilerTestCase{
        .{
            .input = "-1",
            .expected_constants = &.{.{ .int = 1 }},
            .expected_instructions = &.{
                op_constant_0,
                op_minus,
                op_pop,
            },
        },
        .{
            .input = "!true",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_true,
                op_bang,
                op_pop,
            },
        },
        .{
            .input = "!false",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_false,
                op_bang,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "conditionals" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_true = try code.make(allocator, .op_true, &.{});
    defer allocator.free(op_true);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);
    const op_null = try code.make(allocator, .op_null, &.{});
    defer allocator.free(op_null);
    const op_constant_2 = try code.make(allocator, .constant, &.{2});
    defer allocator.free(op_constant_2);
    const op_jump_not_truthy_10 = try code.make(allocator, .jump_not_truthy, &.{10});
    defer allocator.free(op_jump_not_truthy_10);
    const op_jump_11 = try code.make(allocator, .jump, &.{11});
    defer allocator.free(op_jump_11);
    const op_jump_13 = try code.make(allocator, .jump, &.{13});
    defer allocator.free(op_jump_13);

    const tests = [_]CompilerTestCase{
        .{
            .input = "if (true) { 10 }; 3333;",
            .expected_constants = &.{ .{ .int = 10 }, .{ .int = 3333 } },
            .expected_instructions = &.{
                op_true,
                op_jump_not_truthy_10,
                op_constant_0,
                op_jump_11,
                op_null,
                op_pop,
                op_constant_1,
                op_pop,
            },
        },
        .{
            .input = "if (true) { 10 } else { 20 }; 3333;",
            .expected_constants = &.{ .{ .int = 10 }, .{ .int = 20 }, .{ .int = 3333 } },
            .expected_instructions = &.{
                op_true,
                op_jump_not_truthy_10,
                op_constant_0,
                op_jump_13,
                op_constant_1,
                op_pop,
                op_constant_2,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "let statements" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_set_global_0 = try code.make(allocator, .set_global, &.{0});
    defer allocator.free(op_set_global_0);
    const op_set_global_1 = try code.make(allocator, .set_global, &.{1});
    defer allocator.free(op_set_global_1);
    const op_get_global_0 = try code.make(allocator, .get_global, &.{0});
    defer allocator.free(op_get_global_0);
    const op_get_global_1 = try code.make(allocator, .get_global, &.{1});
    defer allocator.free(op_get_global_1);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);

    const tests = [_]CompilerTestCase{
        .{
            .input = "let one = 1; let two = 2;",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 } },
            .expected_instructions = &.{
                op_constant_0,
                op_set_global_0,
                op_constant_1,
                op_set_global_1,
            },
        },
        .{
            .input = "let one = 1; one;",
            .expected_constants = &.{.{ .int = 1 }},
            .expected_instructions = &.{
                op_constant_0,
                op_set_global_0,
                op_get_global_0,
                op_pop,
            },
        },
        .{
            .input = "let one = 1; let two = one; two;",
            .expected_constants = &.{.{ .int = 1 }},
            .expected_instructions = &.{
                op_constant_0,
                op_set_global_0,
                op_get_global_0,
                op_set_global_1,
                op_get_global_1,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "string expressions" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_add = try code.make(allocator, .add, &.{});
    defer allocator.free(op_add);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);

    const tests = [_]CompilerTestCase{
        .{
            .input = "\"monkey\"",
            .expected_constants = &.{.{ .string = "monkey" }},
            .expected_instructions = &.{
                op_constant_0,
                op_pop,
            },
        },
        .{
            .input = "\"mon\" + \"key\"",
            .expected_constants = &.{ .{ .string = "mon" }, .{ .string = "key" } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_add,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "hash literals" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_constant_2 = try code.make(allocator, .constant, &.{2});
    defer allocator.free(op_constant_2);
    const op_constant_3 = try code.make(allocator, .constant, &.{3});
    defer allocator.free(op_constant_3);
    const op_constant_4 = try code.make(allocator, .constant, &.{4});
    defer allocator.free(op_constant_4);
    const op_constant_5 = try code.make(allocator, .constant, &.{5});
    defer allocator.free(op_constant_5);
    const op_add = try code.make(allocator, .add, &.{});
    defer allocator.free(op_add);
    const op_mul = try code.make(allocator, .mul, &.{});
    defer allocator.free(op_mul);
    const op_hash_0 = try code.make(allocator, .hash, &.{0});
    defer allocator.free(op_hash_0);
    const op_hash_6 = try code.make(allocator, .hash, &.{6});
    defer allocator.free(op_hash_6);
    const op_hash_4 = try code.make(allocator, .hash, &.{4});
    defer allocator.free(op_hash_4);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);

    const tests = [_]CompilerTestCase{
        // {}
        .{
            .input = "{}",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_hash_0,
                op_pop,
            },
        },
        // {1: 2, 3: 4, 5: 6}
        .{
            .input = "{1: 2, 3: 4, 5: 6}",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 3 }, .{ .int = 4 }, .{ .int = 5 }, .{ .int = 6 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_constant_2,
                op_constant_3,
                op_constant_4,
                op_constant_5,
                op_hash_6,
                op_pop,
            },
        },
        // {1: 2 + 3, 4: 5 * 6}
        .{
            .input = "{1: 2 + 3, 4: 5 * 6}",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 3 }, .{ .int = 4 }, .{ .int = 5 }, .{ .int = 6 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_constant_2,
                op_add,
                op_constant_3,
                op_constant_4,
                op_constant_5,
                op_mul,
                op_hash_4,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "array literals" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_constant_2 = try code.make(allocator, .constant, &.{2});
    defer allocator.free(op_constant_2);
    const op_constant_3 = try code.make(allocator, .constant, &.{3});
    defer allocator.free(op_constant_3);
    const op_constant_4 = try code.make(allocator, .constant, &.{4});
    defer allocator.free(op_constant_4);
    const op_constant_5 = try code.make(allocator, .constant, &.{5});
    defer allocator.free(op_constant_5);
    const op_add = try code.make(allocator, .add, &.{});
    defer allocator.free(op_add);
    const op_sub = try code.make(allocator, .sub, &.{});
    defer allocator.free(op_sub);
    const op_mul = try code.make(allocator, .mul, &.{});
    defer allocator.free(op_mul);
    const op_array_0 = try code.make(allocator, .array, &.{0});
    defer allocator.free(op_array_0);
    const op_array_3 = try code.make(allocator, .array, &.{3});
    defer allocator.free(op_array_3);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);

    const tests = [_]CompilerTestCase{
        // []
        .{
            .input = "[]",
            .expected_constants = &.{},
            .expected_instructions = &.{
                op_array_0,
                op_pop,
            },
        },
        // [1, 2, 3]
        .{
            .input = "[1, 2, 3]",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 3 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_constant_2,
                op_array_3,
                op_pop,
            },
        },
        // [1 + 2, 3 - 4, 5 * 6]
        .{
            .input = "[1 + 2, 3 - 4, 5 * 6]",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 3 }, .{ .int = 4 }, .{ .int = 5 }, .{ .int = 6 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_add,
                op_constant_2,
                op_constant_3,
                op_sub,
                op_constant_4,
                op_constant_5,
                op_mul,
                op_array_3,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "index expressions" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_constant_2 = try code.make(allocator, .constant, &.{2});
    defer allocator.free(op_constant_2);
    const op_constant_3 = try code.make(allocator, .constant, &.{3});
    defer allocator.free(op_constant_3);
    const op_constant_4 = try code.make(allocator, .constant, &.{4});
    defer allocator.free(op_constant_4);
    const op_add = try code.make(allocator, .add, &.{});
    defer allocator.free(op_add);
    const op_sub = try code.make(allocator, .sub, &.{});
    defer allocator.free(op_sub);
    const op_array_3 = try code.make(allocator, .array, &.{3});
    defer allocator.free(op_array_3);
    const op_hash_2 = try code.make(allocator, .hash, &.{2});
    defer allocator.free(op_hash_2);
    const op_index = try code.make(allocator, .index, &.{});
    defer allocator.free(op_index);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);

    const tests = [_]CompilerTestCase{
        .{
            .input = "[1, 2, 3][1 + 1]",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 3 }, .{ .int = 1 }, .{ .int = 1 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_constant_2,
                op_array_3,
                op_constant_3,
                op_constant_4,
                op_add,
                op_index,
                op_pop,
            },
        },
        .{
            .input = "{1: 2}[2 - 1]",
            .expected_constants = &.{ .{ .int = 1 }, .{ .int = 2 }, .{ .int = 2 }, .{ .int = 1 } },
            .expected_instructions = &.{
                op_constant_0,
                op_constant_1,
                op_hash_2,
                op_constant_2,
                op_constant_3,
                op_sub,
                op_index,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "function literal expressions" {
    const allocator = testing.allocator;
    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_constant_2 = try code.make(allocator, .constant, &.{2});
    defer allocator.free(op_constant_2);
    const op_add = try code.make(allocator, .add, &.{});
    defer allocator.free(op_add);
    const op_return_value = try code.make(allocator, .return_value, &.{});
    defer allocator.free(op_return_value);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);
    const op_return = try code.make(allocator, .op_return, &.{});
    defer allocator.free(op_return);

    const tests = [_]CompilerTestCase{
        .{
            .input = "fn() { return 5 + 10; }",
            .expected_constants = &.{
                .{ .int = 5 }, .{ .int = 10 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_0,
                        op_constant_1,
                        op_add,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_constant_2,
                op_pop,
            },
        },
        .{
            .input = "fn() {  5 + 10; }",
            .expected_constants = &.{
                .{ .int = 5 }, .{ .int = 10 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_0,
                        op_constant_1,
                        op_add,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_constant_2,
                op_pop,
            },
        },

        .{
            .input = "fn() {   }",
            .expected_constants = &.{
                .{
                    .instructions = &[_]code.Instructions{
                        op_return,
                    },
                },
            },
            .expected_instructions = &.{
                op_constant_0,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "function calls" {
    const allocator = testing.allocator;

    const op_constant_0 = try code.make(allocator, .constant, &.{0});
    defer allocator.free(op_constant_0);
    const op_constant_1 = try code.make(allocator, .constant, &.{1});
    defer allocator.free(op_constant_1);
    const op_return_value = try code.make(allocator, .return_value, &.{});
    defer allocator.free(op_return_value);
    const op_call = try code.make(allocator, .call, &.{0});
    defer allocator.free(op_call);
    const op_pop = try code.make(allocator, .pop, &.{});
    defer allocator.free(op_pop);
    const op_set_global_0 = try code.make(allocator, .set_global, &.{0});
    defer allocator.free(op_set_global_0);
    const op_get_global_0 = try code.make(allocator, .get_global, &.{0});
    defer allocator.free(op_get_global_0);

    const tests = [_]CompilerTestCase{
        .{
            .input = "fn() { 24 }();",
            .expected_constants = &.{
                .{ .int = 24 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_0,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_constant_1,
                op_call,
                op_pop,
            },
        },
        .{
            .input =
            \\let noArg = fn() { 24 };
            \\noArg();
            ,
            .expected_constants = &.{
                .{ .int = 24 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_0,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_constant_1,
                op_set_global_0,
                op_get_global_0,
                op_call,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}
