const std = @import("std");
const token = @import("./token.zig");
const parsing_utils = @import("./parsing_utils.zig");

const ArrayList = std.ArrayList;
const Token = token.Token;
const TokenType = token.TokenType;
const Precedence = parsing_utils.Precedence;

pub const Node = struct {
    ptr: *const anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        toString: *const fn (self: *const anyopaque, writer: *std.Io.Writer) std.Io.Writer.Error!void,
    };

    pub fn implBy(impl_obj: anytype) Node {
        const delegator = NodeDelegator(impl_obj);
        return .{
            .ptr = impl_obj,
            .vtable = &.{
                .toString = delegator.toString,
            },
        };
    }

    pub fn toString(self: *const Node, writer: *std.Io.Writer) !void {
        try self.vtable.toString(self.ptr, writer);
    }
};

inline fn NodeDelegator(impl_obj: anytype) type {
    const ImplType = @TypeOf(impl_obj);
    return struct {
        fn toString(self: *const anyopaque, writer: *std.Io.Writer) !void {
            const impl: ImplType = @ptrCast(@alignCast(self));
            try impl.*.toString(writer);
        }
    };
}

pub const ExpNode = struct {
    ptr: *const anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        toString: *const fn (self: *const anyopaque, writer: *std.Io.Writer) std.Io.Writer.Error!void,
        tokenLiteral: *const fn (self: *const anyopaque) []const u8,
    };

    pub fn implBy(impl_obj: anytype) ExpNode {
        const delegator = ExpNodeDelegator(impl_obj);
        return .{
            .ptr = impl_obj,
            .vtable = &.{
                .toString = delegator.toString,
                .tokenLiteral = delegator.tokenLiteral,
            },
        };
    }

    pub fn toString(self: *const ExpNode, writer: *std.Io.Writer) !void {
        try self.vtable.toString(self.ptr, writer);
    }

    pub fn tokenLiteral(self: *const ExpNode) []const u8 {
        return self.vtable.tokenLiteral(self.ptr);
    }
};

inline fn ExpNodeDelegator(impl_obj: anytype) type {
    const ImplType = @TypeOf(impl_obj);
    return struct {
        pub fn toString(self: *const anyopaque, writer: *std.Io.Writer) !void {
            const impl: ImplType = @ptrCast(@alignCast(self));
            try impl.*.toString(writer);
        }

        pub fn tokenLiteral(self: *const anyopaque) []const u8 {
            const impl: ImplType = @ptrCast(@alignCast(self));
            return impl.*.tokenLiteral();
        }
    };
}

pub const ExpressionType = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    float_literal: FloatLiteral,
    boolean_literal: BooleanLiteral,
    function_literal: FunctionLiteral,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    if_expression: IfExpression,
    call_expression: CallExpression,
    string_literal: StringLiteral,
    assign_expression: AssignExpression,
    array_literal: ArrayLiteral,
    index_expression: IndexExpression,
    hash_literal: HashLiteral,

    pub fn toString(self: *const ExpressionType, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
            inline else => |*val| {
                try val.toString(writer);
            },
        }
    }

    pub fn clone(self: *const ExpressionType, allocator: std.mem.Allocator) std.mem.Allocator.Error!ExpressionType {
        return switch (self.*) {
            .identifier => |ident| .{ .identifier = try ident.clone(allocator) },
            .integer_literal => |lit| .{ .integer_literal = .{
                .token = try lit.token.clone(allocator),
                .value = lit.value,
            } },
            .float_literal => |lit| .{ .float_literal = .{
                .token = try lit.token.clone(allocator),
                .value = lit.value,
            } },
            .boolean_literal => |lit| .{ .boolean_literal = .{
                .token = try lit.token.clone(allocator),
                .value = lit.value,
            } },
            .string_literal => |lit| .{ .string_literal = .{
                .token = try lit.token.clone(allocator),
                .value = try allocator.dupe(u8, lit.value),
            } },
            .prefix_expression => |prefix| blk: {
                const right = try allocator.create(StatementType.ExpressionStatement);
                right.* = try prefix.right.clone(allocator);
                const cloned_token = try prefix.token.clone(allocator);
                break :blk .{ .prefix_expression = .{
                    .token = cloned_token,
                    .operator = cloned_token.ch,
                    .right = right,
                } };
            },
            .infix_expression => |infix| blk: {
                const left = try allocator.create(StatementType.ExpressionStatement);
                left.* = try infix.left.clone(allocator);
                const right = try allocator.create(StatementType.ExpressionStatement);
                right.* = try infix.right.clone(allocator);
                const cloned_token = try infix.token.clone(allocator);
                break :blk .{ .infix_expression = .{
                    .token = cloned_token,
                    .operator = cloned_token.ch,
                    .left = left,
                    .right = right,
                } };
            },
            .if_expression => |if_exp| blk: {
                const condition = try allocator.create(StatementType.ExpressionStatement);
                condition.* = try if_exp.condition.clone(allocator);
                break :blk .{ .if_expression = .{
                    .token = try if_exp.token.clone(allocator),
                    .condition = condition,
                    .consequence = try if_exp.consequence.clone(allocator),
                    .alternative = if (if_exp.alternative) |alt| try alt.clone(allocator) else null,
                } };
            },
            .function_literal => |func| blk: {
                const params = try allocator.alloc(Identifier, func.parameters.len);
                for (func.parameters, 0..) |param, i| {
                    params[i] = try param.clone(allocator);
                }
                break :blk .{ .function_literal = .{
                    .token = try func.token.clone(allocator),
                    .parameters = params,
                    .body = try func.body.clone(allocator),
                    .name = func.name,
                } };
            },
            .call_expression => |call| blk: {
                const function = try allocator.create(StatementType.ExpressionStatement);
                function.* = try call.function.clone(allocator);
                const arguments = try allocator.alloc(StatementType.ExpressionStatement, call.arguments.len);
                for (call.arguments, 0..) |*arg, i| {
                    arguments[i] = try arg.clone(allocator);
                }
                break :blk .{ .call_expression = .{
                    .token = try call.token.clone(allocator),
                    .function = function,
                    .arguments = arguments,
                } };
            },
            .assign_expression => |assign| blk: {
                const value = try allocator.create(StatementType.ExpressionStatement);
                value.* = try assign.value.clone(allocator);
                break :blk .{ .assign_expression = .{
                    .token = try assign.token.clone(allocator),
                    .name = try assign.name.clone(allocator),
                    .value = value,
                } };
            },
            .array_literal => |arr| blk: {
                const elements = try allocator.alloc(StatementType.ExpressionStatement, arr.elements.len);
                for (arr.elements, 0..) |*elem, i| {
                    elements[i] = try elem.clone(allocator);
                }
                break :blk .{ .array_literal = .{
                    .token = try arr.token.clone(allocator),
                    .elements = elements,
                } };
            },
            .index_expression => |idx| blk: {
                const left = try allocator.create(StatementType.ExpressionStatement);
                left.* = try idx.left.clone(allocator);
                const index = try allocator.create(StatementType.ExpressionStatement);
                index.* = try idx.index.clone(allocator);
                break :blk .{ .index_expression = .{
                    .token = try idx.token.clone(allocator),
                    .left = left,
                    .index = index,
                } };
            },
            .hash_literal => |hash| blk: {
                const pairs = try allocator.alloc(HashLiteral.HashPair, hash.pairs.len);
                for (hash.pairs, 0..) |*pair, i| {
                    pairs[i] = .{
                        .key = try pair.key.clone(allocator),
                        .value = try pair.value.clone(allocator),
                    };
                }
                break :blk .{ .hash_literal = .{
                    .token = try hash.token.clone(allocator),
                    .pairs = pairs,
                } };
            },
        };
    }

    pub fn initStringLiteral(tkn: Token, value: []const u8) ExpressionType {
        return .{
            .string_literal = .{
                .token = tkn,
                .value = value,
            },
        };
    }

    pub fn initIfExpression(tkn: Token, condition: *StatementType.ExpressionStatement, consequence: StatementType.BlockStatement, alternative: ?StatementType.BlockStatement) ExpressionType {
        return .{
            .if_expression = IfExpression.init(tkn, condition, consequence, alternative),
        };
    }

    pub fn initIdentifier(tkn: Token, value: []const u8) ExpressionType {
        return .{
            .identifier = Identifier.init(tkn, value),
        };
    }

    pub fn initIntegerLiteral(tkn: Token, val: i64) ExpressionType {
        return .{
            .integer_literal = IntegerLiteral.init(tkn, val),
        };
    }

    pub fn initFloatLiteral(tkn: Token, val: f64) ExpressionType {
        return .{ .float_literal = FloatLiteral.init(tkn, val) };
    }

    pub fn initPrefixExpression(tkn: Token, right: *StatementType.ExpressionStatement) ExpressionType {
        return .{
            .prefix_expression = PrefixExpression.init(tkn, right),
        };
    }

    pub fn initInfixExpression(tkn: Token, left: *StatementType.ExpressionStatement, right: *StatementType.ExpressionStatement) ExpressionType {
        return .{
            .infix_expression = InfixExpression.init(tkn, left, right),
        };
    }

    pub fn initBooleanLiteral(tkn: Token, val: bool) ExpressionType {
        return .{
            .boolean_literal = BooleanLiteral.init(tkn, val),
        };
    }

    pub fn initFunctionLiteral(tkn: Token, params: []Identifier, body: StatementType.BlockStatement) ExpressionType {
        return .{
            .function_literal = .{
                .token = tkn,

                .parameters = params,
                .body = body,
            },
        };
    }

    pub fn initCallExpression(tkn: Token, function: *StatementType.ExpressionStatement, arguments: []StatementType.ExpressionStatement) ExpressionType {
        return .{
            .call_expression = CallExpression.init(tkn, function, arguments),
        };
    }

    pub fn initAssignExpression(tkn: Token, name: Identifier, value: *StatementType.ExpressionStatement) ExpressionType {
        return .{
            .assign_expression = AssignExpression.init(tkn, name, value),
        };
    }

    pub fn initArrayLiteral(tkn: Token, elements: []StatementType.ExpressionStatement) ExpressionType {
        return .{
            .array_literal = ArrayLiteral.init(tkn, elements),
        };
    }

    pub fn initIndexExpression(tkn: Token, left: *StatementType.ExpressionStatement, index: *StatementType.ExpressionStatement) ExpressionType {
        return .{
            .index_expression = IndexExpression.init(tkn, left, index),
        };
    }

    pub fn initHashLiteral(tkn: Token, pairs: []HashLiteral.HashPair) ExpressionType {
        return .{
            .hash_literal = HashLiteral.init(tkn, pairs),
        };
    }

    pub const StringLiteral = struct {
        token: Token,
        value: []const u8,

        const Self = @This();

        pub fn init(tkn: Token, value: []const u8) Self {
            return .{
                .token = tkn,
                .value = value,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            try writer.print("{s}", .{self.value});
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const FunctionLiteral = struct {
        token: Token,
        parameters: []Identifier,
        body: StatementType.BlockStatement,
        name: ?[]const u8 = null,

        const Self = @This();

        pub fn init(tkn: Token, parameters: []Identifier, body: StatementType.BlockStatement) Self {
            return .{
                .token = tkn,
                .parameters = parameters,
                .body = body,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            try writer.print("{s}", .{self.token.ch});
            if (self.name) |name| {
                writer.print(" {s}(", .{name}) catch unreachable;
            } else {
                try writer.writeAll("(");
            }
            for (self.parameters, 0..) |param, i| {
                try param.toString(writer);
                if (i < self.parameters.len - 1) {
                    try writer.writeAll(", ");
                }
            }
            _ = try writer.write(") ");
            try self.body.toString(writer);
            try writer.flush();
        }
    };

    pub const IfExpression = struct {
        const Self = @This();
        token: Token,
        condition: *StatementType.ExpressionStatement,
        consequence: StatementType.BlockStatement,
        alternative: ?StatementType.BlockStatement,

        pub fn init(
            tkn: Token,
            cond: *StatementType.ExpressionStatement,
            cons: StatementType.BlockStatement,
            alt: ?StatementType.BlockStatement,
        ) Self {
            return .{
                .token = tkn,
                .condition = cond,
                .consequence = cons,
                .alternative = alt,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            try writer.print("if", .{});
            try self.condition.expression.toString(writer);
            try writer.print(" ", .{});

            try self.consequence.toString(writer);

            if (self.alternative) |alt_block| {
                _ = try writer.write("else");
                try alt_block.toString(writer);
            }
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const BooleanLiteral = struct {
        const Self = @This();
        token: Token,
        value: bool,

        pub fn init(obj: Token, val: bool) Self {
            return .{ .token = obj, .value = val };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            _ = try writer.write(self.tokenLiteral());
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const InfixExpression = struct {
        const Self = @This();
        token: Token,
        left: *StatementType.ExpressionStatement,
        right: *StatementType.ExpressionStatement,
        operator: []const u8,

        pub fn init(obj: Token, left: *StatementType.ExpressionStatement, right: *StatementType.ExpressionStatement) Self {
            return .{
                .token = obj,
                .operator = obj.ch,
                .left = left,
                .right = right,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            _ = try writer.write("(");

            try self.left.expression.toString(writer);
            _ = try writer.print(" {s} ", .{self.token.ch});

            try self.right.expression.toString(writer);

            _ = try writer.write(")");

            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const PrefixExpression = struct {
        const Self = @This();
        token: Token,
        operator: []const u8,
        right: *StatementType.ExpressionStatement,

        pub fn init(obj: Token, right: *StatementType.ExpressionStatement) Self {
            return .{
                .token = obj,
                .operator = obj.ch,
                .right = right,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            try writer.print("({s}", .{self.operator});
            try self.right.expression.toString(writer);
            try writer.print(")", .{});
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const IntegerLiteral = struct {
        const Self = @This();
        token: Token,
        value: i64,

        pub fn init(obj: Token, val: i64) Self {
            return .{
                .token = obj,
                .value = val,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            _ = try writer.write(self.tokenLiteral());
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const FloatLiteral = struct {
        const Self = @This();
        token: Token,
        value: f64,

        pub fn init(obj: Token, val: f64) Self {
            return .{ .token = obj, .value = val };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            _ = try writer.write(self.tokenLiteral());
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const CallExpression = struct {
        const Self = @This();
        token: Token,
        function: *StatementType.ExpressionStatement,
        arguments: []StatementType.ExpressionStatement,

        pub fn init(tkn: Token, function: *StatementType.ExpressionStatement, arguments: []StatementType.ExpressionStatement) Self {
            return .{
                .token = tkn,
                .function = function,
                .arguments = arguments,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            try self.function.expression.toString(writer);
            _ = try writer.write("(");
            for (self.arguments, 0..) |*arg, i| {
                try arg.expression.toString(writer);
                if (i < self.arguments.len - 1) {
                    _ = try writer.write(", ");
                }
            }
            _ = try writer.write(")");
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const AssignExpression = struct {
        const Self = @This();
        token: Token,
        name: Identifier,
        value: *StatementType.ExpressionStatement,

        pub fn init(tkn: Token, name: Identifier, value: *StatementType.ExpressionStatement) Self {
            return .{
                .token = tkn,
                .name = name,
                .value = value,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            try self.name.toString(writer);
            _ = try writer.write(" = ");
            try self.value.expression.toString(writer);
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const ArrayLiteral = struct {
        const Self = @This();
        token: Token,
        elements: []StatementType.ExpressionStatement,

        pub fn init(tkn: Token, elements: []StatementType.ExpressionStatement) Self {
            return .{
                .token = tkn,
                .elements = elements,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            _ = try writer.write("[");
            for (self.elements, 0..) |*elem, i| {
                try elem.expression.toString(writer);
                if (i < self.elements.len - 1) {
                    _ = try writer.write(", ");
                }
            }
            _ = try writer.write("]");
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const IndexExpression = struct {
        const Self = @This();
        token: Token,
        left: *StatementType.ExpressionStatement,
        index: *StatementType.ExpressionStatement,

        pub fn init(tkn: Token, left: *StatementType.ExpressionStatement, index: *StatementType.ExpressionStatement) Self {
            return .{
                .token = tkn,
                .left = left,
                .index = index,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            _ = try writer.write("(");
            try self.left.expression.toString(writer);
            _ = try writer.write("[");
            try self.index.expression.toString(writer);
            _ = try writer.write("])");
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };

    pub const HashLiteral = struct {
        const Self = @This();

        pub const HashPair = struct {
            key: StatementType.ExpressionStatement,
            value: StatementType.ExpressionStatement,
        };

        token: Token,
        pairs: []HashPair,

        pub fn init(tkn: Token, pairs: []HashPair) Self {
            return .{
                .token = tkn,
                .pairs = pairs,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            _ = try writer.write("{");
            for (self.pairs, 0..) |*pair, i| {
                try pair.key.expression.toString(writer);
                _ = try writer.write(":");
                try pair.value.expression.toString(writer);
                if (i < self.pairs.len - 1) {
                    _ = try writer.write(", ");
                }
            }
            _ = try writer.write("}");
            try writer.flush();
        }

        pub fn tokenLiteral(self: *const Self) []const u8 {
            return self.token.ch;
        }
    };
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn init(tkn: Token, value: []const u8) Identifier {
        return .{
            .token = tkn,
            .value = value,
        };
    }

    pub fn clone(self: Identifier, allocator: std.mem.Allocator) std.mem.Allocator.Error!Identifier {
        return .{
            .token = try self.token.clone(allocator),
            .value = try allocator.dupe(u8, self.value),
        };
    }

    pub fn toString(self: *const Identifier, writer: *std.Io.Writer) !void {
        try writer.print("{s}", .{self.value});
        try writer.flush();
    }

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.value;
    }
};

pub const StatementType = union(enum) {
    let: LetStatement,
    @"return": ReturnStatement,
    expression: ExpressionStatement,
    block: BlockStatement,

    pub fn toString(self: *const StatementType, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
            inline else => |*val| try val.toString(writer),
        }
    }

    pub fn clone(self: *const StatementType, allocator: std.mem.Allocator) std.mem.Allocator.Error!StatementType {
        return switch (self.*) {
            .let => |*let_stmt| .{ .let = try let_stmt.clone(allocator) },
            .@"return" => |*ret_stmt| .{ .@"return" = try ret_stmt.clone(allocator) },
            .expression => |*exp_stmt| .{ .expression = try exp_stmt.clone(allocator) },
            .block => |*block_stmt| .{ .block = try block_stmt.clone(allocator) },
        };
    }

    pub fn deinit(self: *StatementType, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .let => |*let_stmt| let_stmt.value.deinit(allocator),
            .@"return" => |*return_stmt| {
                if (return_stmt.value) |*value| {
                    value.deinit(allocator);
                }
            },
            .expression => |*exp_stmt| exp_stmt.deinit(allocator),
            .block => |*block_stmt| block_stmt.deinit(allocator),
        }
    }

    pub const BlockStatement = struct {
        token: Token,
        statements: ArrayList(StatementType) = .empty,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(tkn: Token, allocator: std.mem.Allocator) BlockStatement {
            return .{
                .token = tkn,
                .statements = .empty,
                .allocator = allocator,
            };
        }

        pub fn addStatement(self: *Self, stmt: StatementType) !void {
            try self.statements.append(self.allocator, stmt);
        }

        pub fn clone(self: *const BlockStatement, allocator: std.mem.Allocator) std.mem.Allocator.Error!BlockStatement {
            var new_block = BlockStatement.init(try self.token.clone(allocator), allocator);
            for (self.statements.items) |*stmt| {
                try new_block.addStatement(try stmt.clone(allocator));
            }
            return new_block;
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            for (self.statements.items) |*stmt| {
                try stmt.toString(writer);
            }
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            for (self.statements.items) |*stmt| {
                stmt.deinit(allocator);
            }
            self.statements.deinit(self.allocator);
        }
    };

    pub const ExpressionStatement = struct {
        expression: ExpressionType,

        const Self = @This();

        pub fn initStringLiteral(tkn: Token, value: []const u8) Self {
            return .{
                .expression = ExpressionType.initStringLiteral(tkn, value),
            };
        }

        pub fn initFunctionLiteral(tkn: Token, params: []Identifier, body: StatementType.BlockStatement) Self {
            return .{
                .expression = ExpressionType.initFunctionLiteral(
                    tkn,
                    params,
                    body,
                ),
            };
        }
        pub fn initIfExpression(tkn: Token, condition: *ExpressionStatement, consequence: StatementType.BlockStatement, alternative: ?StatementType.BlockStatement) Self {
            return .{
                .expression = ExpressionType.initIfExpression(
                    tkn,
                    condition,
                    consequence,
                    alternative,
                ),
            };
        }

        pub fn initIdentifierExpression(tkn: Token, value: []const u8) Self {
            return .{
                .expression = ExpressionType.initIdentifier(tkn, value),
            };
        }

        pub fn initIntegerLiteralExpression(tkn: Token, val: i64) Self {
            return .{
                .expression = ExpressionType.initIntegerLiteral(tkn, val),
            };
        }

        pub fn initFloatLiteralExpression(tkn: Token, val: f64) Self {
            return .{ .expression = ExpressionType.initFloatLiteral(tkn, val) };
        }

        pub fn initPrefixExpression(tkn: Token, right: *StatementType.ExpressionStatement) Self {
            return .{
                .expression = ExpressionType.initPrefixExpression(tkn, right),
            };
        }

        pub fn initInfixExpression(tkn: Token, left: *StatementType.ExpressionStatement, right: *StatementType.ExpressionStatement) Self {
            return .{
                .expression = ExpressionType.initInfixExpression(tkn, left, right),
            };
        }

        pub fn initBooleanLiteralExpression(tkn: Token, value: bool) Self {
            return .{
                .expression = ExpressionType.initBooleanLiteral(tkn, value),
            };
        }

        pub fn initCallExpression(tkn: Token, function: *ExpressionStatement, arguments: []ExpressionStatement) Self {
            return .{
                .expression = ExpressionType.initCallExpression(tkn, function, arguments),
            };
        }

        pub fn initAssignExpression(tkn: Token, name: Identifier, value: *ExpressionStatement) Self {
            return .{
                .expression = ExpressionType.initAssignExpression(tkn, name, value),
            };
        }

        pub fn initArrayLiteral(tkn: Token, elements: []ExpressionStatement) Self {
            return .{
                .expression = ExpressionType.initArrayLiteral(tkn, elements),
            };
        }

        pub fn initIndexExpression(tkn: Token, left: *ExpressionStatement, index: *ExpressionStatement) Self {
            return .{
                .expression = ExpressionType.initIndexExpression(tkn, left, index),
            };
        }

        pub fn initHashLiteral(tkn: Token, pairs: []ExpressionType.HashLiteral.HashPair) Self {
            return .{
                .expression = ExpressionType.initHashLiteral(tkn, pairs),
            };
        }

        pub fn clone(self: *const ExpressionStatement, allocator: std.mem.Allocator) std.mem.Allocator.Error!ExpressionStatement {
            return .{ .expression = try self.expression.clone(allocator) };
        }

        pub fn toString(self: *const ExpressionStatement, writer: *std.Io.Writer) !void {
            try self.expression.toString(writer);
            try writer.flush();
        }

        pub fn deinit(self: *ExpressionStatement, allocator: std.mem.Allocator) void {
            switch (self.expression) {
                .prefix_expression => |*prefix| {
                    prefix.right.deinit(allocator);
                    allocator.destroy(prefix.right);
                },
                .infix_expression => |*infix| {
                    infix.left.deinit(allocator);
                    allocator.destroy(infix.left);
                    infix.right.deinit(allocator);
                    allocator.destroy(infix.right);
                },
                .if_expression => |*if_exp| {
                    if_exp.condition.deinit(allocator);
                    allocator.destroy(if_exp.condition);
                    if_exp.consequence.deinit(allocator);
                    if (if_exp.alternative) |*alt| {
                        alt.deinit(allocator);
                    }
                },
                .function_literal => |*fun_exp| {
                    fun_exp.body.deinit(allocator);
                    allocator.free(fun_exp.parameters);
                },
                .call_expression => |*call_exp| {
                    call_exp.function.deinit(allocator);
                    allocator.destroy(call_exp.function);
                    for (call_exp.arguments) |*arg| {
                        arg.deinit(allocator);
                    }
                    allocator.free(call_exp.arguments);
                },
                .assign_expression => |*assign_exp| {
                    assign_exp.value.deinit(allocator);
                    allocator.destroy(assign_exp.value);
                },
                .array_literal => |*arr| {
                    for (arr.elements) |*elem| {
                        elem.deinit(allocator);
                    }
                    allocator.free(arr.elements);
                },
                .index_expression => |*idx| {
                    idx.left.deinit(allocator);
                    allocator.destroy(idx.left);
                    idx.index.deinit(allocator);
                    allocator.destroy(idx.index);
                },
                .hash_literal => |*hash| {
                    for (hash.pairs) |*pair| {
                        pair.key.deinit(allocator);
                        pair.value.deinit(allocator);
                    }
                    allocator.free(hash.pairs);
                },
                else => {},
            }
        }
    };

    pub const ReturnStatement = struct {
        token: Token,
        value: ?ExpressionStatement,

        pub fn clone(self: *const ReturnStatement, allocator: std.mem.Allocator) std.mem.Allocator.Error!ReturnStatement {
            return .{
                .token = try self.token.clone(allocator),
                .value = if (self.value) |*val| try val.clone(allocator) else null,
            };
        }

        pub fn toString(self: *const ReturnStatement, writer: *std.Io.Writer) !void {
            try writer.print("{s}", .{
                self.token.token_type.toString(),
            });

            if (self.value) |*value| {
                try writer.writeAll(" ");
                _ = try value.expression.toString(writer);
            }

            _ = try writer.write(";");
            try writer.flush();
        }
    };

    pub const LetStatement = struct {
        token: Token,
        name: Identifier,
        value: ExpressionStatement,

        pub fn clone(self: *const LetStatement, allocator: std.mem.Allocator) std.mem.Allocator.Error!LetStatement {
            return .{
                .token = try self.token.clone(allocator),
                .name = try self.name.clone(allocator),
                .value = try self.value.clone(allocator),
            };
        }

        pub fn toString(self: *const LetStatement, writer: *std.Io.Writer) !void {
            try writer.print("{s} ", .{self.token.token_type.toString()});

            try self.name.toString(writer);

            _ = try writer.write(" = ");

            try self.value.expression.toString(writer);
            _ = try writer.write(";");
            try writer.flush();
        }
    };
};

pub const Program = struct {
    statements: ArrayList(StatementType),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{ .allocator = allocator, .statements = .empty };
    }

    pub fn toString(self: *const Program, writer: *std.Io.Writer) !void {
        for (self.statements.items) |*stmt| {
            try stmt.toString(writer);
        }
    }

    pub fn addStatement(self: *Program, statement: StatementType) !void {
        try self.statements.append(self.allocator, statement);
    }

    pub fn deinit(self: *Program) void {
        for (self.statements.items) |*stmt| {
            stmt.deinit(self.allocator);
        }
        self.statements.deinit(self.allocator);
    }
};
