const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const object = @import("object.zig");

const ExpressionType = ast.ExpressionType;
const Program = ast.Program;
const StatementType = ast.StatementType;
const Object = object.Object;

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

const TRUE: Object = .{ .bool = Object.Boolean.init(true) };
const FALSE: Object = .{ .bool = Object.Boolean.init(false) };
const NULL: Object = .{ .null = Object.Null.init() };

pub fn eval(node: anytype) ?Object {
    return switch (@TypeOf(node)) {
        *Program, *const Program => evalProgram(node),
        *StatementType, *const StatementType => evalStatement(node),
        *StatementType.ExpressionStatement, *const StatementType.ExpressionStatement => evalExpressionStatement(node),
        *ExpressionType, *const ExpressionType => evalExpression(node),
        inline else => @compileError("unsupported node type for eval"),
    };
}

fn evalProgram(program: *const Program) ?Object {
    return evalStatements(program.statements.items);
}

fn evalStatements(statements: []const StatementType) ?Object {
    var result: ?Object = null;

    for (statements) |*statement| {
        result = evalStatement(statement);
    }

    return result;
}

fn evalStatement(statement: *const StatementType) ?Object {
    return switch (statement.*) {
        .expression => |*expression_statement| evalExpressionStatement(expression_statement),
        else => null,
    };
}

fn evalExpressionStatement(expression_statement: *const StatementType.ExpressionStatement) ?Object {
    if (expression_statement.expression) |*expression| {
        return evalExpression(expression);
    }

    return null;
}

fn evalExpression(expression: *const ExpressionType) ?Object {
    return switch (expression.*) {
        .integer_literal => |integer_literal| .{
            .int = Object.Integer.init(integer_literal.value),
        },
        .boolean_literal => |boolean_literal| if (boolean_literal.value) TRUE else FALSE,
        .prefix_expression => |*prefix_expression| {
            const right = evalExpression(&prefix_expression.*.right.expression.?) orelse return null;
            return evalPrefixExpression(prefix_expression, right);
        },
        else => null,
    };
}

fn evalPrefixExpression(operator: *const ExpressionType.PrefixExpression, right: Object) ?Object {
    return switch (operator.token.token_type) {
        .bang => return evalBangOperatorExpression(right),
        else => null,
    };
}

fn evalBangOperatorExpression(right: Object) Object {
    return switch (right) {
        .bool => |boolean| if (boolean.value) FALSE else TRUE,
        .null => TRUE,
        else => FALSE,
    };
}

fn testIntegerObjectEqual(obj: ?Object, expected: i64) !void {
    const obj_value = obj orelse return error.TestUnexpectedResult;

    const integer = switch (obj_value) {
        .int => |value| value,
        else => {
            std.debug.print("Expected an int. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testing.expectEqual(expected, integer.value);
}

fn testBooleanObjectEqual(obj: ?Object, expected: bool) !void {
    const obj_value = obj orelse return error.TestUnexpectedResult;

    const boolean = switch (obj_value) {
        .bool => |value| value,
        else => {
            std.debug.print("Expected an bool. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testing.expectEqual(expected, boolean.value);
}

fn testEval(input: []const u8, allocator: std.mem.Allocator) !?Object {
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parse();
    return eval(program);
}

test "eval integer expression" {
    const tests = [_]struct {
        input: []const u8,
        expected: i64,
    }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "5; 10;", .expected = 10 },
    };

    for (tests) |case| {
        const evaluated = try testEval(case.input, testing.allocator);
        try testIntegerObjectEqual(evaluated, case.expected);
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
    };

    for (tests) |case| {
        const evaluated = try testEval(case.input, testing.allocator);
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
        const evaluated = try testEval(case.input, testing.allocator);
        try testBooleanObjectEqual(evaluated, case.expected);
    }
}
