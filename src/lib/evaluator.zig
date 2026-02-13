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
        else => null,
    };
}

fn testIntegerObjectEqual(obj: ?Object, expected: i64) !void {
    const obj_value = obj orelse return error.TestUnexpectedResult;

    const integer = switch (obj_value) {
        .int => |value| value,
    };

    try testing.expectEqual(expected, integer.value);
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
