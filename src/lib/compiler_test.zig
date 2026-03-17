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

fn makeOp(alloc: std.mem.Allocator, op: code.Opcode, operands: []const usize) []u8 {
    return code.make(alloc, op, operands) catch @panic("OOM in test");
}

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

        var symbol_table = SymbolTable.init(allocator, null);
        defer symbol_table.deinit();
        var constants = std.ArrayList(Object).empty;
        defer constants.deinit(allocator);
        var comp = Compiler.init(allocator, &symbol_table, &constants);
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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_add = makeOp(a, .add, &.{});
    const op_sub = makeOp(a, .sub, &.{});
    const op_mul = makeOp(a, .mul, &.{});
    const op_div = makeOp(a, .div, &.{});
    const op_pop = makeOp(a, .pop, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_true = makeOp(a, .op_true, &.{});
    const op_false = makeOp(a, .op_false, &.{});
    const op_pop = makeOp(a, .pop, &.{});
    const op_equal = makeOp(a, .equal, &.{});
    const op_not_equal = makeOp(a, .not_equal, &.{});
    const op_greater_than = makeOp(a, .greater_than, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_true = makeOp(a, .op_true, &.{});
    const op_false = makeOp(a, .op_false, &.{});
    const op_pop = makeOp(a, .pop, &.{});
    const op_minus = makeOp(a, .minus, &.{});
    const op_bang = makeOp(a, .bang, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_true = makeOp(a, .op_true, &.{});
    const op_pop = makeOp(a, .pop, &.{});
    const op_null = makeOp(a, .op_null, &.{});
    const op_constant_2 = makeOp(a, .constant, &.{2});
    const op_jump_not_truthy_10 = makeOp(a, .jump_not_truthy, &.{10});
    const op_jump_11 = makeOp(a, .jump, &.{11});
    const op_jump_13 = makeOp(a, .jump, &.{13});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_set_global_0 = makeOp(a, .set_global, &.{0});
    const op_set_global_1 = makeOp(a, .set_global, &.{1});
    const op_get_global_0 = makeOp(a, .get_global, &.{0});
    const op_get_global_1 = makeOp(a, .get_global, &.{1});
    const op_pop = makeOp(a, .pop, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_add = makeOp(a, .add, &.{});
    const op_pop = makeOp(a, .pop, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_constant_2 = makeOp(a, .constant, &.{2});
    const op_constant_3 = makeOp(a, .constant, &.{3});
    const op_constant_4 = makeOp(a, .constant, &.{4});
    const op_constant_5 = makeOp(a, .constant, &.{5});
    const op_add = makeOp(a, .add, &.{});
    const op_mul = makeOp(a, .mul, &.{});
    const op_hash_0 = makeOp(a, .hash, &.{0});
    const op_hash_6 = makeOp(a, .hash, &.{6});
    const op_hash_4 = makeOp(a, .hash, &.{4});
    const op_pop = makeOp(a, .pop, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_constant_2 = makeOp(a, .constant, &.{2});
    const op_constant_3 = makeOp(a, .constant, &.{3});
    const op_constant_4 = makeOp(a, .constant, &.{4});
    const op_constant_5 = makeOp(a, .constant, &.{5});
    const op_add = makeOp(a, .add, &.{});
    const op_sub = makeOp(a, .sub, &.{});
    const op_mul = makeOp(a, .mul, &.{});
    const op_array_0 = makeOp(a, .array, &.{0});
    const op_array_3 = makeOp(a, .array, &.{3});
    const op_pop = makeOp(a, .pop, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_constant_2 = makeOp(a, .constant, &.{2});
    const op_constant_3 = makeOp(a, .constant, &.{3});
    const op_constant_4 = makeOp(a, .constant, &.{4});
    const op_add = makeOp(a, .add, &.{});
    const op_sub = makeOp(a, .sub, &.{});
    const op_array_3 = makeOp(a, .array, &.{3});
    const op_hash_2 = makeOp(a, .hash, &.{2});
    const op_index = makeOp(a, .index, &.{});
    const op_pop = makeOp(a, .pop, &.{});

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
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_add = makeOp(a, .add, &.{});
    const op_return_value = makeOp(a, .return_value, &.{});
    const op_pop = makeOp(a, .pop, &.{});
    const op_return = makeOp(a, .op_return, &.{});
    const op_closure_2_0 = makeOp(a, .closure, &.{ 2, 0 });
    const op_closure_0_0 = makeOp(a, .closure, &.{ 0, 0 });

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
                op_closure_2_0,
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
                op_closure_2_0,
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
                op_closure_0_0,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "let statement scopes" {
    const allocator = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_get_global_0 = makeOp(a, .get_global, &.{0});
    const op_set_global_0 = makeOp(a, .set_global, &.{0});
    const op_set_local_0 = makeOp(a, .set_local, &.{0});
    const op_set_local_1 = makeOp(a, .set_local, &.{1});
    const op_get_local_0 = makeOp(a, .get_local, &.{0});
    const op_get_local_1 = makeOp(a, .get_local, &.{1});
    const op_return_value = makeOp(a, .return_value, &.{});
    const op_pop = makeOp(a, .pop, &.{});
    const op_add = makeOp(a, .add, &.{});
    const op_closure_1_0 = makeOp(a, .closure, &.{ 1, 0 });
    const op_closure_2_0 = makeOp(a, .closure, &.{ 2, 0 });

    const tests = [_]CompilerTestCase{
        .{
            .input = "let num = 55; fn() { num }",
            .expected_constants = &.{
                .{ .int = 55 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_global_0,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_constant_0,
                op_set_global_0,
                op_closure_1_0,
                op_pop,
            },
        },
        .{
            .input = "fn() { let num = 55; num }",
            .expected_constants = &.{
                .{ .int = 55 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_0,
                        op_set_local_0,
                        op_get_local_0,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_closure_1_0,
                op_pop,
            },
        },
        .{
            .input = "fn() { let a = 55; let b = 77; a + b }",
            .expected_constants = &.{
                .{ .int = 55 },
                .{ .int = 77 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_0,
                        op_set_local_0,
                        op_constant_1,
                        op_set_local_1,
                        op_get_local_0,
                        op_get_local_1,
                        op_add,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_closure_2_0,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "function calls" {
    const allocator = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_return_value = makeOp(a, .return_value, &.{});
    const op_call_0 = makeOp(a, .call, &.{0});
    const op_call_1 = makeOp(a, .call, &.{1});
    const op_call_3 = makeOp(a, .call, &.{3});
    const op_pop = makeOp(a, .pop, &.{});
    const op_set_global_0 = makeOp(a, .set_global, &.{0});
    const op_get_global_0 = makeOp(a, .get_global, &.{0});
    const op_get_local_0 = makeOp(a, .get_local, &.{0});
    const op_get_local_1 = makeOp(a, .get_local, &.{1});
    const op_get_local_2 = makeOp(a, .get_local, &.{2});
    const op_constant_2 = makeOp(a, .constant, &.{2});
    const op_constant_3 = makeOp(a, .constant, &.{3});
    const op_closure_1_0 = makeOp(a, .closure, &.{ 1, 0 });
    const op_closure_0_0 = makeOp(a, .closure, &.{ 0, 0 });

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
                op_closure_1_0,
                op_call_0,
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
                op_closure_1_0,
                op_set_global_0,
                op_get_global_0,
                op_call_0,
                op_pop,
            },
        },
        .{
            .input =
            \\let oneArg = fn(a) { a };
            \\oneArg(24);
            ,
            .expected_constants = &.{
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_local_0,
                        op_return_value,
                    },
                },
                .{ .int = 24 },
            },
            .expected_instructions = &.{
                op_closure_0_0,
                op_set_global_0,
                op_get_global_0,
                op_constant_1,
                op_call_1,
                op_pop,
            },
        },
        .{
            .input =
            \\let manyArg = fn(a, b, c) { a; b; c };
            \\manyArg(24, 25, 26);
            ,
            .expected_constants = &.{
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_local_0,
                        op_pop,
                        op_get_local_1,
                        op_pop,
                        op_get_local_2,
                        op_return_value,
                    },
                },
                .{ .int = 24 },
                .{ .int = 25 },
                .{ .int = 26 },
            },
            .expected_instructions = &.{
                op_closure_0_0,
                op_set_global_0,
                op_get_global_0,
                op_constant_1,
                op_constant_2,
                op_constant_3,
                op_call_3,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "closures" {
    const allocator = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_get_free_0 = makeOp(a, .get_free, &.{0});
    const op_get_free_1 = makeOp(a, .get_free, &.{1});
    const op_get_local_0 = makeOp(a, .get_local, &.{0});
    const op_add = makeOp(a, .add, &.{});
    const op_return_value = makeOp(a, .return_value, &.{});
    const op_closure_0_1 = makeOp(a, .closure, &.{ 0, 1 });
    const op_closure_0_2 = makeOp(a, .closure, &.{ 0, 2 });
    const op_closure_1_0 = makeOp(a, .closure, &.{ 1, 0 });
    const op_closure_1_1 = makeOp(a, .closure, &.{ 1, 1 });
    const op_closure_2_0 = makeOp(a, .closure, &.{ 2, 0 });
    const op_closure_4_2 = makeOp(a, .closure, &.{ 4, 2 });
    const op_closure_5_1 = makeOp(a, .closure, &.{ 5, 1 });
    const op_closure_6_0 = makeOp(a, .closure, &.{ 6, 0 });
    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_1 = makeOp(a, .constant, &.{1});
    const op_constant_2 = makeOp(a, .constant, &.{2});
    const op_constant_3 = makeOp(a, .constant, &.{3});
    const op_set_local_0 = makeOp(a, .set_local, &.{0});
    const op_get_global_0 = makeOp(a, .get_global, &.{0});
    const op_set_global_0 = makeOp(a, .set_global, &.{0});
    const op_pop = makeOp(a, .pop, &.{});

    const tests = [_]CompilerTestCase{
        .{
            .input =
            \\fn(a) {
            \\    fn(b) {
            \\        a + b
            \\    }
            \\}
            ,
            .expected_constants = &.{
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_free_0,
                        op_get_local_0,
                        op_add,
                        op_return_value,
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_local_0,
                        op_closure_0_1,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_closure_1_0,
                op_pop,
            },
        },
        .{
            .input =
            \\fn(a) {
            \\    fn(b) {
            \\        fn(c) {
            \\            a + b + c
            \\        }
            \\    }
            \\}
            ,
            .expected_constants = &.{
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_free_0,
                        op_get_free_1,
                        op_add,
                        op_get_local_0,
                        op_add,
                        op_return_value,
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_free_0,
                        op_get_local_0,
                        op_closure_0_2,
                        op_return_value,
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        op_get_local_0,
                        op_closure_1_1,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_closure_2_0,
                op_pop,
            },
        },
        .{
            .input =
            \\let global = 55;
            \\
            \\fn() {
            \\    let a = 66;
            \\
            \\    fn() {
            \\        let b = 77;
            \\
            \\        fn() {
            \\            let c = 88;
            \\
            \\            global + a + b + c;
            \\        }
            \\    }
            \\}
            ,
            .expected_constants = &.{
                .{ .int = 55 },
                .{ .int = 66 },
                .{ .int = 77 },
                .{ .int = 88 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_3,
                        op_set_local_0,
                        op_get_global_0,
                        op_get_free_0,
                        op_add,
                        op_get_free_1,
                        op_add,
                        op_get_local_0,
                        op_add,
                        op_return_value,
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_2,
                        op_set_local_0,
                        op_get_free_0,
                        op_get_local_0,
                        op_closure_4_2,
                        op_return_value,
                    },
                },
                .{
                    .instructions = &[_]code.Instructions{
                        op_constant_1,
                        op_set_local_0,
                        op_get_local_0,
                        op_closure_5_1,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_constant_0,
                op_set_global_0,
                op_closure_6_0,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}

test "recursive functions" {
    const allocator = testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const op_current_closure = makeOp(a, .current_closure, &.{});
    const op_get_local_0 = makeOp(a, .get_local, &.{0});
    const op_constant_0 = makeOp(a, .constant, &.{0});
    const op_constant_2 = makeOp(a, .constant, &.{2});
    const op_sub = makeOp(a, .sub, &.{});
    const op_call_1 = makeOp(a, .call, &.{1});
    const op_return_value = makeOp(a, .return_value, &.{});
    const op_closure_1_0 = makeOp(a, .closure, &.{ 1, 0 });
    const op_closure_3_0 = makeOp(a, .closure, &.{ 3, 0 });
    const op_set_global_0 = makeOp(a, .set_global, &.{0});
    const op_get_global_0 = makeOp(a, .get_global, &.{0});
    const op_set_local_0 = makeOp(a, .set_local, &.{0});
    const op_call_0 = makeOp(a, .call, &.{0});
    const op_pop = makeOp(a, .pop, &.{});

    const tests = [_]CompilerTestCase{
        .{
            .input =
                \\let countDown = fn(x) { countDown(x - 1); };
                \\countDown(1);
            ,
            .expected_constants = &.{
                .{ .int = 1 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_current_closure,
                        op_get_local_0,
                        op_constant_0,
                        op_sub,
                        op_call_1,
                        op_return_value,
                    },
                },
                .{ .int = 1 },
            },
            .expected_instructions = &.{
                op_closure_1_0,
                op_set_global_0,
                op_get_global_0,
                op_constant_2,
                op_call_1,
                op_pop,
            },
        },
        .{
            .input =
                \\let wrapper = fn() {
                \\    let countDown = fn(x) { countDown(x - 1); };
                \\    countDown(1);
                \\};
                \\wrapper();
            ,
            .expected_constants = &.{
                .{ .int = 1 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_current_closure,
                        op_get_local_0,
                        op_constant_0,
                        op_sub,
                        op_call_1,
                        op_return_value,
                    },
                },
                .{ .int = 1 },
                .{
                    .instructions = &[_]code.Instructions{
                        op_closure_1_0,
                        op_set_local_0,
                        op_get_local_0,
                        op_constant_2,
                        op_call_1,
                        op_return_value,
                    },
                },
            },
            .expected_instructions = &.{
                op_closure_3_0,
                op_set_global_0,
                op_get_global_0,
                op_call_0,
                op_pop,
            },
        },
    };

    try runCompilerTests(allocator, &tests);
}
