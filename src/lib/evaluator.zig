const std = @import("std");

const ast = @import("ast.zig");
const object = @import("object.zig");
const environment = @import("environment.zig");
const gc = @import("gc.zig");

const ExpressionType = ast.ExpressionType;
const Program = ast.Program;
const Identifier = ast.Identifier;
const StatementType = ast.StatementType;
const Object = object.Object;
const Environment = environment.Environment;
const Gc = gc.Gc;

const BuiltinMapType = std.StaticStringMap(Object);
const builtins = BuiltinMapType.initComptime(.{
    .{
        "len", Object{ .builtin = Object.Builtin.init(&len) },
    },
});

fn len(self: *Evaluator, args: []const Object) std.mem.Allocator.Error!Object {
    if (args.len != 1) {
        return try self.errorObj("wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .string => |s| .{ .int = Object.Integer.init(@intCast(s.value.len)) },
        .array => |a| .{ .int = Object.Integer.init(@intCast(a.elements.len)) },
        else => return try self.errorObj("argument to `len` not supported, got {s}", .{args[0].typeName()}),
    };
}

pub const Evaluator = struct {
    environment: *Environment,
    gc: *Gc,
    last_error: ?Object.Error = null,

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

    pub fn init(env: *Environment, collector: *Gc) Self {
        return .{
            .environment = env,
            .gc = collector,
            .last_error = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.freeLastError();
    }

    fn freeLastError(self: *Self) void {
        if (self.last_error) |err| {
            self.gc.allocator.free(err.msg);
            self.last_error = null;
        }
    }

    pub fn eval(self: *Self, node: anytype) EvalError!Object {
        self.freeLastError();
        return unwrapEvalResult(try self.evalNode(node));
    }

    fn evalNode(self: *Self, node: anytype) EvalError!EvalResult {
        return switch (@TypeOf(node)) {
            *Program, *const Program => try self.evalProgram(node),
            *StatementType, *const StatementType => try self.evalStatement(node),
            *StatementType.ExpressionStatement, *const StatementType.ExpressionStatement => try self.evalExpressionStatement(node),
            *ExpressionType, *const ExpressionType => try self.evalExpression(node),
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

    fn unwrapValue(result: EvalResult) ?Object {
        return switch (result) {
            .value => |v| v,
            else => null,
        };
    }

    inline fn wrapResult(tag: EvalResultTag, value: Object) EvalResult {
        return switch (tag) {
            .value => .{ .value = value },
            .return_value => .{ .return_value = value },
            .err => .{ .err = value },
        };
    }

    fn errorObj(self: *Self, comptime format: []const u8, args: anytype) EvalError!Object {
        const msg = try std.fmt.allocPrint(self.gc.allocator, format, args);
        self.freeLastError();
        self.last_error = Object.Error.init(msg);
        return .{ .err = self.last_error.? };
    }

    fn evalProgram(self: *Self, program: *const Program) EvalError!EvalResult {
        return try self.evalStatements(program.statements.items);
    }

    fn evalStatements(self: *Self, statements: []const StatementType) EvalError!EvalResult {
        var result = wrapResult(.value, NULL);

        for (statements) |*statement| {
            result = try self.evalStatement(statement);

            switch (result) {
                .return_value, .err => return result,
                else => {},
            }
        }

        return result;
    }

    fn evalBlockStatement(self: *Self, block_statement: *const StatementType.BlockStatement) EvalError!EvalResult {
        return try self.evalStatements(block_statement.statements.items);
    }

    fn evalStatement(self: *Self, statement: *const StatementType) EvalError!EvalResult {
        return switch (statement.*) {
            .let => |*let_statement| try self.evalLetStatement(let_statement),
            .expression => |*expression_statement| try self.evalExpressionStatement(expression_statement),
            .@"return" => |*return_statement| try self.evalReturnStatement(return_statement),
            .block => |*block_statement| try self.evalBlockStatement(block_statement),
        };
    }

    fn evalLetStatement(self: *Self, let_statement: *const StatementType.LetStatement) EvalError!EvalResult {
        const value_result = try self.evalExpressionStatement(&let_statement.value);
        const value = unwrapValue(value_result) orelse return value_result;

        _ = try self.gc.envSet(self.environment, let_statement.name.token.ch, value);
        return wrapResult(.value, NULL);
    }

    fn evalReturnStatement(self: *Self, return_statement: *const StatementType.ReturnStatement) EvalError!EvalResult {
        const return_expr = return_statement.value orelse return wrapResult(.return_value, NULL);
        const value_result = try self.evalExpressionStatement(&return_expr);

        return switch (value_result) {
            .value => |value| wrapResult(.return_value, value),
            .return_value => |value| wrapResult(.return_value, value),
            .err => |value| wrapResult(.err, value),
        };
    }

    fn evalExpressionStatement(self: *Self, expression_statement: *const StatementType.ExpressionStatement) EvalError!EvalResult {
        return try self.evalExpression(&expression_statement.expression);
    }

    fn evalExpression(self: *Self, expression: *const ExpressionType) EvalError!EvalResult {
        return switch (expression.*) {
            .integer_literal => |integer_literal| wrapResult(.value, .{
                .int = Object.Integer.init(integer_literal.value),
            }),
            .function_literal => |*function_literal| try self.evalFunctionLiteral(function_literal),
            .call_expression => |*call_expression| try self.evalCallExpression(call_expression),
            .identifier => |*identifier_statement| wrapResult(.value, try self.evalIdentifierStatement(identifier_statement)),
            .boolean_literal => |boolean_literal| wrapResult(.value, if (boolean_literal.value) TRUE else FALSE),
            .prefix_expression => |*prefix_expression| try self.evalPrefixExpression(prefix_expression),
            .infix_expression => |*infix_expression| try self.evalInfixExpression(infix_expression),
            .if_expression => |*if_expression| try self.evalIfExpression(if_expression),
            .string_literal => |string_literal| wrapResult(.value, try self.gc.allocString(string_literal.value)),
            .assign_expression => |*assign_exp| try self.evalAssignExpression(assign_exp),
            .array_literal => |*arr| try self.evalArrayLiteral(arr),
            .index_expression => |*idx| try self.evalIndexExpression(idx),
        };
    }

    fn evalArrayLiteral(self: *Self, arr: *const ExpressionType.ArrayLiteral) EvalError!EvalResult {
        const elements = try self.gc.allocator.alloc(Object, arr.elements.len);
        defer self.gc.allocator.free(elements);

        for (arr.elements, 0..) |*elem, i| {
            const result = try self.evalExpressionStatement(elem);
            const value = unwrapValue(result) orelse return result;
            elements[i] = value;
        }

        const array_obj = try self.gc.allocArray(elements);
        return wrapResult(.value, array_obj);
    }

    fn evalIndexExpression(self: *Self, idx: *const ExpressionType.IndexExpression) EvalError!EvalResult {
        const left_result = try self.evalExpressionStatement(idx.left);
        const left = unwrapValue(left_result) orelse return left_result;

        const index_result = try self.evalExpressionStatement(idx.index);
        const index = unwrapValue(index_result) orelse return index_result;

        switch (left) {
            .array => |array_obj| {
                switch (index) {
                    .int => |int_obj| {
                        if (int_obj.value < 0) return wrapResult(.value, NULL);
                        const i: usize = @intCast(int_obj.value);
                        if (i >= array_obj.elements.len) return wrapResult(.value, NULL);
                        return wrapResult(.value, array_obj.elements[i]);
                    },
                    else => return wrapResult(.err, try self.errorObj("index operator not supported: {s}", .{index.typeName()})),
                }
            },
            else => return wrapResult(.err, try self.errorObj("index operator not supported: {s}", .{left.typeName()})),
        }
    }

    fn evalFunctionLiteral(self: *Self, function_literal: *const ExpressionType.FunctionLiteral) EvalError!EvalResult {
        var func_arena = std.heap.ArenaAllocator.init(self.gc.allocator);
        errdefer func_arena.deinit();
        const arena_alloc = func_arena.allocator();

        const cloned_params = try arena_alloc.alloc(ast.Identifier, function_literal.parameters.len);
        for (function_literal.parameters, 0..) |param, i| {
            cloned_params[i] = try param.clone(arena_alloc);
        }

        const cloned_body = try arena_alloc.create(ast.StatementType.BlockStatement);
        cloned_body.* = try function_literal.body.clone(arena_alloc);

        const allocated_function = try self.gc.allocFunction(Object.Function.init(
            cloned_params,
            cloned_body,
            self.environment,
            func_arena,
        ));

        return wrapResult(.value, allocated_function);
    }

    fn evalPrefixExpression(self: *Self, prefix_expression: *const ExpressionType.PrefixExpression) EvalError!EvalResult {
        const right_result = try self.evalExpression(&prefix_expression.right.expression);
        const right = unwrapValue(right_result) orelse return right_result;

        const evaluated = try self.applyPrefixOperator(prefix_expression, right);
        return switch (evaluated) {
            .err => wrapResult(.err, evaluated),
            else => wrapResult(.value, evaluated),
        };
    }

    fn evalInfixExpression(self: *Self, infix_expression: *const ExpressionType.InfixExpression) EvalError!EvalResult {
        const left_result = try self.evalExpression(&infix_expression.left.expression);
        const left = unwrapValue(left_result) orelse return left_result;

        const right_result = try self.evalExpression(&infix_expression.right.expression);
        const right = unwrapValue(right_result) orelse return right_result;

        const evaluated = try self.applyInfixOperator(infix_expression, &left, &right);
        return switch (evaluated) {
            .err => wrapResult(.err, evaluated),
            else => wrapResult(.value, evaluated),
        };
    }

    fn evalAssignExpression(self: *Self, assign_exp: *const ExpressionType.AssignExpression) EvalError!EvalResult {
        const value_result = try self.evalExpression(&assign_exp.value.expression);
        const value = unwrapValue(value_result) orelse return value_result;

        if (self.gc.envSetExisting(self.environment, assign_exp.name.value, value)) |result| {
            return wrapResult(.value, result);
        } else {
            return wrapResult(.err, try self.errorObj("identifier not found: {s}", .{assign_exp.name.value}));
        }
    }

    fn evalCallExpression(self: *Self, call_expression: *const ExpressionType.CallExpression) EvalError!EvalResult {
        const function_result = try self.evalExpression(&call_expression.function.expression);
        const function = unwrapValue(function_result) orelse return function_result;

        switch (function) {
            .err => return wrapResult(.err, function),
            else => {},
        }

        const arguments = try self.gc.allocator.alloc(Object, call_expression.arguments.len);
        defer self.gc.allocator.free(arguments);

        for (call_expression.arguments, 0..) |*argument, i| {
            const argument_result = try self.evalExpression(&argument.expression);
            const evaluated_argument = unwrapValue(argument_result) orelse return argument_result;

            switch (evaluated_argument) {
                .err => return wrapResult(.err, evaluated_argument),
                else => {},
            }

            arguments[i] = evaluated_argument;
        }

        const evaluated = try self.applyFunction(function, arguments);
        return switch (evaluated) {
            .err => wrapResult(.err, evaluated),
            else => wrapResult(.value, evaluated),
        };
    }

    fn applyFunction(self: *Self, func: Object, args: []const Object) EvalError!Object {
        return switch (func) {
            .function => |function| {
                if (args.len != function.parameters.len) {
                    return try self.errorObj(
                        "wrong number of arguments: want={d}, got={d}",
                        .{ function.parameters.len, args.len },
                    );
                }

                const previous_env = self.environment;
                const extended_environment = try self.gc.allocEnvironment(function.environment);

                for (function.parameters, 0..) |parameter, i| {
                    _ = try self.gc.envSet(extended_environment, parameter.value, args[i]);
                }

                self.environment = extended_environment;
                defer self.environment = previous_env;

                const evaluated = try self.evalBlockStatement(function.body);
                return unwrapEvalResult(evaluated);
            },
            .builtin => |builtin| {
                return try builtin.function(self, args);
            },
            else => try self.errorObj("not a function: {s}", .{func.typeName()}),
        };
    }

    fn evalIdentifierStatement(self: *Self, expression: *const Identifier) !Object {
        var value = self.environment.get(expression.token.ch);
        if (value) |v| {
            return v;
        }

        value = builtins.get(expression.token.ch);
        if (value) |v| {
            return v;
        }
        return try self.errorObj("identifier not found: {s}", .{expression.token.ch});
    }

    fn applyInfixOperator(self: *Self, expression: *const ExpressionType.InfixExpression, left: *const Object, right: *const Object) !Object {
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
            .string => |right_ptr| switch (left.*) {
                .string => |left_ptr| try self.evalStringInfixExpression(expression, left_ptr, right_ptr),
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

    fn evalStringInfixExpression(self: *Self, expression: *const ExpressionType.InfixExpression, left: *Object.String, right: *Object.String) !Object {
        return switch (expression.token.token_type) {
            .plus => try self.gc.allocStringConcat(left.value, right.value),
            .eq => if (std.mem.eql(u8, left.value, right.value)) TRUE else FALSE,
            .not_eq => if (!std.mem.eql(u8, left.value, right.value)) TRUE else FALSE,
            else => self.errorObj("unknown operator {s} {s} {s}", .{
                "STRING",
                expression.token.ch,
                "STRING",
            }),
        };
    }

    fn applyPrefixOperator(self: *Self, operator: *const ExpressionType.PrefixExpression, right: Object) EvalError!Object {
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

    fn evalIfExpression(self: *Self, expression: *const ExpressionType.IfExpression) EvalError!EvalResult {
        const condition_result = try self.evalNode(expression.condition);
        const condition = unwrapValue(condition_result) orelse return condition_result;

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
