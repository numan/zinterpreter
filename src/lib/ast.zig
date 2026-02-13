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
    boolean_literal: BooleanLiteral,
    function_literal: FunctionLiteral,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    if_expression: IfExpression,
    call_expression: CallExpression,

    pub fn toString(self: *const ExpressionType, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
            inline else => |*val| {
                try val.toString(writer);
            },
        }
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

    pub const FunctionLiteral = struct {
        token: Token,
        parameters: []Identifier,
        body: StatementType.BlockStatement,

        const Self = @This();

        pub fn init(tkn: Token, parameters: []Identifier, body: StatementType.BlockStatement) Self {
            return .{
                .token = tkn,
                .parameters = parameters,
                .body = body,
            };
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            try writer.print("{s}(", .{self.token.ch});
            for (self.parameters, 0..) |param, i| {
                try param.toString(writer);
                if (i < self.parameters.len - 1) {
                    _ = try writer.write(", ");
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
            if (self.condition.*.expression) |*cond_exp| {
                try cond_exp.toString(writer);
            }
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

            if (self.left.*.expression) |*left_expression| {
                try left_expression.toString(writer);
            }
            _ = try writer.print(" {s} ", .{self.token.ch});

            if (self.right.*.expression) |*right_expression| {
                try right_expression.toString(writer);
            }

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
            if (self.right.*.expression) |*exp| {
                try exp.toString(writer);
            }
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
            if (self.function.expression) |*func_exp| {
                try func_exp.toString(writer);
            }
            _ = try writer.write("(");
            for (self.arguments, 0..) |*arg, i| {
                if (arg.expression) |*exp| {
                    try exp.toString(writer);
                }
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

    pub fn toString(self: *const Identifier, writer: *std.Io.Writer) !void {
        try writer.print("{s}", .{self.value});
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

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            for (self.statements.items) |*stmt| {
                try stmt.toString(writer);
            }
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            for (self.statements.items) |*stmt| {
                switch (stmt.*) {
                    .expression => |*exp_stmt| exp_stmt.deinit(allocator),
                    else => {},
                }
            }
            self.statements.deinit(self.allocator);
        }
    };

    pub const ExpressionStatement = struct {
        expression: ?ExpressionType,

        const Self = @This();

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

        pub fn toString(self: *const ExpressionStatement, writer: *std.Io.Writer) !void {
            if (self.expression) |*value| {
                try value.toString(writer);
            } else {
                _ = try writer.write("");
            }
            try writer.flush();
        }

        pub fn deinit(self: *ExpressionStatement, allocator: std.mem.Allocator) void {
            if (self.expression) |*exp_node| {
                switch (exp_node.*) {
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
                    else => {},
                }
            }
        }
    };

    pub const ReturnStatement = struct {
        token: Token,
        value: ExpressionStatement,

        pub fn toString(self: *const ReturnStatement, writer: *std.Io.Writer) !void {
            try writer.print("{s} ", .{
                self.token.token_type.toString(),
            });

            if (self.value.expression) |*value| {
                _ = try value.toString(writer);
            } else {
                _ = try writer.write("");
            }

            _ = try writer.write(";");
            try writer.flush();
        }
    };

    pub const LetStatement = struct {
        token: Token,
        name: Identifier,
        value: ExpressionStatement,

        pub fn toString(self: *const LetStatement, writer: *std.Io.Writer) !void {
            try writer.print("{s} ", .{self.token.token_type.toString()});

            try self.name.toString(writer);

            _ = try writer.write(" = ");

            if (self.value.expression) |*value| {
                try value.toString(writer);
            } else {
                _ = try writer.write("");
            }
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
            switch (stmt.*) {
                .expression => |*exp_stmt| exp_stmt.deinit(self.allocator),
                else => {},
            }
        }
        self.statements.deinit(self.allocator);
    }
};
