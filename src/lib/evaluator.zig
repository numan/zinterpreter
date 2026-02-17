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
        .infix_expression => |*infix_expression| {
            const left = evalExpression(&infix_expression.*.left.expression.?) orelse return null;
            const right = evalExpression(&infix_expression.*.right.expression.?) orelse return null;
            return evalInfixExpression(infix_expression, &left, &right);
        },
        .if_expression => |*if_expression| return evalIfExpression(if_expression),
        else => null,
    };
}

fn evalInfixExpression(expression: *const ExpressionType.InfixExpression, left: *const Object, right: *const Object) Object {
    return switch (right.*) {
        .int => |*right_ptr| switch (left.*) {
            .int => |*left_ptr| evalIntInfixExpression(expression, left_ptr, right_ptr),
            else => NULL,
        },
        .bool => |*right_ptr| switch (left.*) {
            .bool => |*left_ptr| evalBoolInfixExpression(expression, left_ptr, right_ptr),
            else => NULL,
        },
        else => NULL,
    };
}

fn evalIntInfixExpression(expression: *const ExpressionType.InfixExpression, left: *const Object.Integer, right: *const Object.Integer) Object {
    return switch (expression.token.token_type) {
        .plus => .{ .int = Object.Integer.init(left.value + right.value) },
        .minus => .{ .int = Object.Integer.init(left.value - right.value) },
        .asterisk => .{ .int = Object.Integer.init(left.value * right.value) },
        .slash => .{ .int = Object.Integer.init(@divTrunc(left.value, right.value)) },
        .lt => if (left.value < right.value) TRUE else FALSE,
        .gt => if (left.value > right.value) TRUE else FALSE,
        .eq => if (left.value == right.value) TRUE else FALSE,
        .not_eq => if (left.value != right.value) TRUE else FALSE,
        else => NULL,
    };
}
fn evalBoolInfixExpression(expression: *const ExpressionType.InfixExpression, left: *const Object.Boolean, right: *const Object.Boolean) Object {
    return switch (expression.token.token_type) {
        .eq => if (left.value == right.value) TRUE else FALSE,
        .not_eq => if (left.value != right.value) TRUE else FALSE,
        else => NULL,
    };
}

fn evalPrefixExpression(operator: *const ExpressionType.PrefixExpression, right: Object) Object {
    return switch (operator.token.token_type) {
        .bang => return evalBangOperatorExpression(right),
        .minus => return evalMinusPrefixOperatorExpression(right),
        else => NULL,
    };
}

fn evalBangOperatorExpression(right: Object) Object {
    return switch (right) {
        .bool => |boolean| if (boolean.value) FALSE else TRUE,
        .null => TRUE,
        else => FALSE,
    };
}

fn evalMinusPrefixOperatorExpression(right: Object) Object {
    return switch (right) {
        .int => |integer| .{ .int = Object.Integer.init(-integer.value) },
        else => NULL,
    };
}

fn evalIfExpression(expression: *const ExpressionType.IfExpression) ?Object {
    const condition = eval(expression.condition) orelse NULL;

    if (isTruthy(condition)) {
        return evalStatements(expression.consequence.statements.items);
    } else if (expression.alternative) |alternative| {
        return evalStatements(alternative.statements.items);
    } else {
        return NULL;
    }
}

fn isTruthy(obj: Object) bool {
    return switch (obj) {
        .null => false,
        .bool => |boolean| boolean.value,
        else => true,
    };
}

fn testNullObject(obj: ?Object) !void {
    const obj_value = obj orelse return error.TestUnexpectedResult;

    switch (obj_value) {
        .null => {},
        else => {
            std.debug.print("Expected null. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    }
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
        const evaluated = try testEval(case.input, testing.allocator);
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
        const evaluated = try testEval(case.input, testing.allocator);
        if (case.expected) |expected| {
            try testIntegerObjectEqual(evaluated, expected);
        } else {
            try testNullObject(evaluated);
        }
    }
}
