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

pub const Evaluator = struct {
    arena: std.heap.ArenaAllocator,

    const Self = @This();
    const EvalError = error{OutOfMemory};

    const TRUE: Object = .{ .bool = Object.Boolean.init(true) };
    const FALSE: Object = .{ .bool = Object.Boolean.init(false) };
    const NULL: Object = .{ .null = Object.Null.init() };

    const EvalResult = union(enum) {
        value: Object,
        return_value: Object,
        err: Object,
    };

    const EvalResultTag = std.meta.Tag(EvalResult);

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }

    pub fn reset(self: *Self) void {
        _ = self.arena.reset(.retain_capacity);
    }

    pub fn eval(self: *Self, node: anytype) EvalError!?Object {
        const result = try self.evalNode(node) orelse return null;
        return unwrapEvalResult(result);
    }

    fn evalNode(self: *Self, node: anytype) EvalError!?EvalResult {
        return switch (@TypeOf(node)) {
            *Program, *const Program => try self.evalProgram(node),
            *StatementType, *const StatementType => self.evalStatement(node),
            *StatementType.ExpressionStatement, *const StatementType.ExpressionStatement => try self.evalExpressionStatement(node),
            *ExpressionType, *const ExpressionType => self.evalExpression(node),
            inline else => @compileError("unsupported node type for eval"),
        };
    }

    inline fn unwrapEvalResult(result: EvalResult) Object {
        return switch (result) {
            .value => |value| value,
            .return_value => |value| value,
            .err => |value| value,
        };
    }

    inline fn wrapResult(tag: EvalResultTag, value: Object) EvalResult {
        return switch (tag) {
            .value => .{ .value = value },
            .return_value => .{ .return_value = value },
            .err => .{ .err = value },
        };
    }

    fn errorMsg(self: *Self, comptime format: []const u8, args: anytype) EvalError![]const u8 {
        return try std.fmt.allocPrint(self.arena.allocator(), format, args);
    }

    fn errorObj(self: *Self, comptime format: []const u8, args: anytype) EvalError!Object {
        return .{
            .err = Object.Error.init(try self.errorMsg(format, args)),
        };
    }

    fn evalProgram(self: *Self, program: *const Program) EvalError!?EvalResult {
        return try self.evalStatements(program.statements.items);
    }

    fn evalStatements(self: *Self, statements: []const StatementType) EvalError!?EvalResult {
        var result: ?EvalResult = null;

        for (statements) |*statement| {
            result = try self.evalStatement(statement);

            if (result) |evaluated| {
                switch (evaluated) {
                    .return_value, .err => return evaluated,
                    else => {},
                }
            }
        }

        return result;
    }

    fn evalBlockStatement(self: *Self, block_statement: *const StatementType.BlockStatement) EvalError!?EvalResult {
        return try self.evalStatements(block_statement.statements.items);
    }

    fn evalStatement(self: *Self, statement: *const StatementType) EvalError!?EvalResult {
        return switch (statement.*) {
            .expression => |*expression_statement| try self.evalExpressionStatement(expression_statement),
            .@"return" => |*return_statement| self.evalReturnStatement(return_statement),
            .block => |*block_statement| self.evalBlockStatement(block_statement),
            else => null,
        };
    }

    fn evalReturnStatement(self: *Self, return_statement: *const StatementType.ReturnStatement) EvalError!?EvalResult {
        const value_result = try self.evalExpressionStatement(&return_statement.value) orelse return wrapResult(.return_value, NULL);

        return switch (value_result) {
            .value => |value| wrapResult(.return_value, value),
            .return_value => |value| wrapResult(.return_value, value),
            .err => |value| wrapResult(.err, value),
        };
    }

    fn evalExpressionStatement(self: *Self, expression_statement: *const StatementType.ExpressionStatement) EvalError!?EvalResult {
        if (expression_statement.expression) |*expression| {
            return try self.evalExpression(expression);
        }

        return null;
    }

    fn evalExpression(self: *Self, expression: *const ExpressionType) EvalError!?EvalResult {
        return switch (expression.*) {
            .integer_literal => |integer_literal| wrapResult(.value, .{
                .int = Object.Integer.init(integer_literal.value),
            }),
            .boolean_literal => |boolean_literal| wrapResult(.value, if (boolean_literal.value) TRUE else FALSE),
            .prefix_expression => |*prefix_expression| {
                const right_result = try self.evalExpression(&prefix_expression.*.right.expression.?) orelse return null;
                const right = switch (right_result) {
                    .value => |value| value,
                    .return_value => |value| return wrapResult(.return_value, value),
                    .err => |value| return wrapResult(.err, value),
                };

                const evaluated = try self.evalPrefixExpression(prefix_expression, right);
                return switch (evaluated) {
                    .err => wrapResult(.err, evaluated),
                    else => wrapResult(.value, evaluated),
                };
            },
            .infix_expression => |*infix_expression| {
                const left_result = try self.evalExpression(&infix_expression.*.left.expression.?) orelse return null;
                const left = switch (left_result) {
                    .value => |value| value,
                    .return_value => |value| return wrapResult(.return_value, value),
                    .err => |value| return wrapResult(.err, value),
                };

                const right_result = try self.evalExpression(&infix_expression.*.right.expression.?) orelse return null;
                const right = switch (right_result) {
                    .value => |value| value,
                    .return_value => |value| return wrapResult(.return_value, value),
                    .err => |value| return wrapResult(.err, value),
                };

                const evaluated = try self.evalInfixExpression(infix_expression, &left, &right);
                return switch (evaluated) {
                    .err => wrapResult(.err, evaluated),
                    else => wrapResult(.value, evaluated),
                };
            },
            .if_expression => |*if_expression| self.evalIfExpression(if_expression),
            else => null,
        };
    }

    fn evalInfixExpression(self: *Self, expression: *const ExpressionType.InfixExpression, left: *const Object, right: *const Object) !Object {
        return switch (right.*) {
            .int => |*right_ptr| switch (left.*) {
                .int => |*left_ptr| evalIntInfixExpression(expression, left_ptr, right_ptr),
                else => self.errorObj("type mismatch: {s} {s} {s}", .{
                    left.typeName(),
                    expression.token.ch,
                    right.typeName(),
                }),
            },
            .bool => |*right_ptr| switch (left.*) {
                .bool => |*left_ptr| switch (expression.token.token_type) {
                    .eq, .not_eq => evalBoolInfixExpression(expression, left_ptr, right_ptr),
                    else => self.errorObj("unknown operator {s} {s} {s}", .{
                        left.typeName(),
                        expression.token.ch,
                        right.typeName(),
                    }),
                },
                else => self.errorObj("type mismatch: {s} {s} {s}", .{
                    left.typeName(),
                    expression.token.ch,
                    right.typeName(),
                }),
            },
            else => self.errorObj("unknown operator {s} {s} {s}", .{
                left.typeName(),
                expression.token.ch,
                right.typeName(),
            }),
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

    fn evalPrefixExpression(self: *Self, operator: *const ExpressionType.PrefixExpression, right: Object) EvalError!Object {
        return switch (operator.token.token_type) {
            .bang => evalBangOperatorExpression(right),
            .minus => try self.evalMinusPrefixOperatorExpression(right),
            else => try self.errorObj("unknown operator: {s}{s}", .{
                operator.token.ch,
                right.typeName(),
            }),
        };
    }

    fn evalBangOperatorExpression(right: Object) Object {
        return switch (right) {
            .bool => |boolean| if (boolean.value) FALSE else TRUE,
            .null => TRUE,
            else => FALSE,
        };
    }

    fn evalMinusPrefixOperatorExpression(self: *Self, right: Object) EvalError!Object {
        return switch (right) {
            .int => |integer| .{ .int = Object.Integer.init(-integer.value) },
            else => try self.errorObj("unknown operator: -{s}", .{right.typeName()}),
        };
    }

    fn evalIfExpression(self: *Self, expression: *const ExpressionType.IfExpression) EvalError!?EvalResult {
        const condition_result: EvalResult = try self.evalNode(expression.condition) orelse wrapResult(.value, NULL);
        const condition = switch (condition_result) {
            .value => |value| value,
            .return_value => |value| return wrapResult(.return_value, value),
            .err => |value| return wrapResult(.err, value),
        };

        if (isTruthy(condition)) {
            return self.evalBlockStatement(&expression.consequence);
        } else if (expression.alternative) |*alternative| {
            return self.evalBlockStatement(alternative);
        } else {
            return wrapResult(.value, NULL);
        }
    }

    fn isTruthy(obj: Object) bool {
        return switch (obj) {
            .null => false,
            .bool => |boolean| boolean.value,
            else => true,
        };
    }
};

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

fn testErrorObjectMessageEqual(obj: ?Object, expected: []const u8) !void {
    const obj_value = obj orelse return error.TestUnexpectedResult;

    switch (obj_value) {
        .err => |err_obj| {
            try testing.expectEqualStrings(expected, err_obj.msg);
        },
        else => {
            std.debug.print(
                "Expected error object for input, got tag {s} instead.\n",
                .{obj_value.typeName()},
            );
            return error.TestUnexpectedResult;
        },
    }
}

fn testEval(input: []const u8, allocator: std.mem.Allocator, evaluator: *Evaluator) !?Object {
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
        var evaluator = Evaluator.init(testing.allocator);
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
        var evaluator = Evaluator.init(testing.allocator);
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
        var evaluator = Evaluator.init(testing.allocator);
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
        var evaluator = Evaluator.init(testing.allocator);
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
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated = try testEval(case.input, testing.allocator, &evaluator);
        try testIntegerObjectEqual(evaluated, case.expected);
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
    };

    for (tests) |case| {
        var evaluator = Evaluator.init(testing.allocator);
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
