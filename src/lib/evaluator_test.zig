const std = @import("std");
const testing = std.testing;

const Object = @import("./object.zig").Object;
const Evaluator = @import("./evaluator.zig").Evaluator;
const Environment = @import("./environment.zig").Environment;
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;

fn testNullObject(obj: Object) !void {
    switch (obj) {
        .null => {},
        else => {
            std.debug.print("Expected null. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    }
}

fn testIntegerObjectEqual(obj: Object, expected: i64) !void {
    const integer = switch (obj) {
        .int => |value| value,
        else => {
            std.debug.print("Expected an int. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testing.expectEqual(expected, integer.value);
}

fn testBooleanObjectEqual(obj: Object, expected: bool) !void {
    const boolean = switch (obj) {
        .bool => |value| value,
        else => {
            std.debug.print("Expected an bool. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testing.expectEqual(expected, boolean.value);
}

fn testErrorObjectMessageEqual(obj: Object, expected: []const u8) !void {
    switch (obj) {
        .err => |err_obj| {
            try testing.expectEqualStrings(expected, err_obj.msg);
        },
        else => {
            std.debug.print(
                "Expected error object for input, got tag {s} instead.\n",
                .{obj.typeName()},
            );
            return error.TestUnexpectedResult;
        },
    }
}

fn testEval(input: []const u8, allocator: std.mem.Allocator, evaluator: *Evaluator) !Object {
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parse();
    return evaluator.eval(program);
}

test "eval integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{
            .input = "5",
            .expected = 5,
        },
        .{
            .input = "10",
            .expected = 10,
        },
        .{
            .input = "5; 10;",
            .expected = 10,
        },
        .{
            .input = "-5",
            .expected = -5,
        },
        .{
            .input = "-10",
            .expected = -10,
        },
        .{
            .input = "5 + 5 + 5 + 5 - 10",
            .expected = 10,
        },
        .{
            .input = "2 * 2 * 2 * 2 * 2",
            .expected = 32,
        },
        .{
            .input = "-50 + 100 + -50",
            .expected = 0,
        },
        .{
            .input = "5 * 2 + 10",
            .expected = 20,
        },
        .{
            .input = "5 + 2 * 10",
            .expected = 25,
        },
        .{
            .input = "20 + 2 * -10",
            .expected = 0,
        },
        .{
            .input = "50 / 2 * 2 + 10",
            .expected = 60,
        },
        .{
            .input = "2 * (5 + 10)",
            .expected = 30,
        },
        .{
            .input = "3 * 3 * 3 + 10",
            .expected = 37,
        },
        .{
            .input = "3 * (3 * 3) + 10",
            .expected = 37,
        },
        .{
            .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            .expected = 50,
        },
    };

    for (tests) |case| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        testIntegerObjectEqual(evaluated, case.expected) catch |err| {
            std.debug.print(
                "Got wrong value for input {s}",
                .{case.input},
            );
            return err;
        };
    }
}

test "eval boolean" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{
            .input = "true",
            .expected = true,
        },
        .{
            .input = "false",
            .expected = false,
        },
        .{
            .input = "1 < 2",
            .expected = true,
        },
        .{
            .input = "1 > 2",
            .expected = false,
        },
        .{
            .input = "1 < 1",
            .expected = false,
        },
        .{
            .input = "1 > 1",
            .expected = false,
        },
        .{
            .input = "1 == 1",
            .expected = true,
        },
        .{
            .input = "1 != 1",
            .expected = false,
        },
        .{
            .input = "1 == 2",
            .expected = false,
        },
        .{
            .input = "1 != 2",
            .expected = true,
        },
        .{
            .input = "true == true",
            .expected = true,
        },
        .{
            .input = "false == false",
            .expected = true,
        },
        .{
            .input = "true == false",
            .expected = false,
        },
        .{
            .input = "true != false",
            .expected = true,
        },
        .{
            .input = "false != true",
            .expected = true,
        },
        .{
            .input = "(1 < 2) == true",
            .expected = true,
        },
        .{
            .input = "(1 < 2) == false",
            .expected = false,
        },
        .{
            .input = "(1 > 2) == true",
            .expected = false,
        },
        .{
            .input = "(1 > 2) == false",
            .expected = true,
        },
    };

    for (tests) |case| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        try testBooleanObjectEqual(evaluated, case.expected);
    }
}

test "bang operator" {
    const tests = [_]struct {
        input: []const u8,
        expected: bool,
    }{
        .{
            .input = "!true",
            .expected = false,
        },
        .{
            .input = "!false",
            .expected = true,
        },
        .{
            .input = "!!true",
            .expected = true,
        },
        .{
            .input = "!!false",
            .expected = false,
        },
        .{
            .input = "!5",
            .expected = false,
        },
        .{
            .input = "!!5",
            .expected = true,
        },
    };

    for (tests) |case| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        try testBooleanObjectEqual(evaluated, case.expected);
    }
}

test "if else expressions" {
    const tests = [_]struct {
        input: []const u8,
        expected: ?i64,
    }{
        .{
            .input = "if (true) { 10 }",
            .expected = 10,
        },
        .{
            .input = "if (false) { 10 }",
            .expected = null,
        },
        .{
            .input = "if (1) { 10 }",
            .expected = 10,
        },
        .{
            .input = "if (1 < 2) { 10 }",
            .expected = 10,
        },
        .{
            .input = "if (1 > 2) { 10 }",
            .expected = null,
        },
        .{
            .input = "if (1 > 2) { 10 } else { 20 }",
            .expected = 20,
        },
        .{
            .input = "if (1 < 2) { 10 } else { 20 }",
            .expected = 10,
        },
    };

    for (tests) |case| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        if (case.expected) |expected| {
            try testIntegerObjectEqual(evaluated, expected);
        } else {
            try testNullObject(evaluated);
        }
    }
}

test "return statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        .{
            .input =
            \\if (10 > 1) {
            \\  if (10 > 1) {
            \\    return 10;
            \\  }
            \\
            \\  return 1;
            \\}
            ,
            .expected = 10,
        },
    };

    for (tests) |case| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        try testIntegerObjectEqual(evaluated, case.expected);
    }
}

test "bare return statements" {
    const tests = [_][]const u8{
        "return;",
        "if (10 > 1) { return; }",
        "if (10 > 1) { if (10 > 1) { return; } return 1; }",
    };

    for (tests) |input| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(input, testing.allocator, &evaluator);
        try testNullObject(evaluated);
    }
}

test "error handling" {
    const tests = [_]struct {
        input: []const u8,
        expected_message: []const u8,
    }{
        .{
            .input = "5 + true;",
            .expected_message = "type mismatch: int + bool",
        },
        .{
            .input = "5 + true; 5;",
            .expected_message = "type mismatch: int + bool",
        },
        .{
            .input = "-true",
            .expected_message = "unknown operator: -bool",
        },
        .{
            .input = "true + false;",
            .expected_message = "unknown operator bool + bool",
        },
        .{
            .input = "5; true + false; 5",
            .expected_message = "unknown operator bool + bool",
        },
        .{
            .input = "if (10 > 1) { true + false; }",
            .expected_message = "unknown operator bool + bool",
        },
        .{
            .input =
            \\if (10 > 1) {
            \\  if (10 > 1) {
            \\    return true + false;
            \\  }
            \\
            \\  return 1;
            \\}
            ,
            .expected_message = "unknown operator bool + bool",
        },
        .{
            .input = "foobar",
            .expected_message = "identifier not found: foobar",
        },
    };

    for (tests) |case| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        testErrorObjectMessageEqual(evaluated, case.expected_message) catch |err| {
            std.debug.print(
                "Got wrong error object for input:\n{s}\n",
                .{case.input},
            );
            return err;
        };
    }
}

test "let statements" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
    };

    for (tests) |case| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var evaluator = Evaluator.init(testing.allocator, &env);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        try testIntegerObjectEqual(evaluated, case.expected);
    }
}
