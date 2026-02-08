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
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    if_expression: IfExpression,

    pub fn toString(self: *const ExpressionType, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
            inline else => |*val| {
                try val.toString(writer);
            },
        }
    }

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
            if (self.condition.*.expression) |cond_exp| {
                const exp_node = switch (cond_exp.expression) {
                    inline else => |*val| ExpNode.implBy(val),
                };
                try exp_node.toString(writer);
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

            if (self.left.*.expression) |left_expression| {
                const exp_node = switch (left_expression.expression) {
                    inline else => |*val| ExpNode.implBy(val),
                };
                try exp_node.toString(writer);
            }
            _ = try writer.print(" {s} ", .{self.token.ch});

            if (self.right.*.expression) |right_expression| {
                const exp_node = switch (right_expression.expression) {
                    inline else => |*val| ExpNode.implBy(val),
                };
                try exp_node.toString(writer);
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
            if (self.right.*.expression) |exp| {
                const exp_node = switch (exp.expression) {
                    inline else => |*val| ExpNode.implBy(val),
                };
                try exp_node.toString(writer);
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
        value: u64,

        pub fn init(obj: Token, val: u64) Self {
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
};

pub const Identifier = struct {
    token_type: TokenType,
    value: []const u8,

    pub fn init(value: []const u8) Identifier {
        return .{ .token_type = .iden, .value = value };
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

    pub const BlockStatement = struct {
        token: Token,
        statements: ArrayList(StatementNode) = .empty,
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn init(tkn: Token, allocator: std.mem.Allocator) BlockStatement {
            return .{
                .token = tkn,
                .statements = .empty,
                .allocator = allocator,
            };
        }

        pub fn addStatement(self: *Self, stmt: StatementNode) !void {
            try self.statements.append(self.allocator, stmt);
        }

        pub fn toString(self: *const Self, writer: *std.Io.Writer) !void {
            for (self.statements.items) |*stmt| {
                try stmt.*.toString(writer);
            }
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            for (self.statements.items) |*stmt| {
                switch (stmt.statement) {
                    .expression => |*exp_stmt| exp_stmt.deinit(allocator),
                    else => {},
                }
            }
            self.statements.deinit(self.allocator);
        }
    };

    pub const ExpressionStatement = struct {
        expression: ?ExpressionNode,

        const Self = @This();

        pub fn initIfExpression(tkn: Token, condition: *ExpressionStatement, consequence: StatementType.BlockStatement, alternative: ?StatementType.BlockStatement) Self {
            return .{
                .expression = ExpressionNode.initIfExpression(tkn, condition, consequence, alternative),
            };
        }

        pub fn initIdentifierExpression(value: []const u8) Self {
            return .{
                .expression = ExpressionNode.initIdentifier(value),
            };
        }

        pub fn initIntegerLiteralExpression(tkn: Token, val: u64) Self {
            return .{
                .expression = ExpressionNode.initIntegerLiteral(tkn, val),
            };
        }

        pub fn initPrefixExpression(tkn: Token, right: *StatementType.ExpressionStatement) Self {
            return .{
                .expression = ExpressionNode.initPrefixExpression(tkn, right),
            };
        }

        pub fn initInfixExpression(tkn: Token, left: *StatementType.ExpressionStatement, right: *StatementType.ExpressionStatement) Self {
            return .{
                .expression = ExpressionNode.initInfixExpression(tkn, left, right),
            };
        }

        pub fn initBooleanLiteralExpression(tkn: Token, value: bool) Self {
            return .{
                .expression = ExpressionNode.initBooleanLiteral(tkn, value),
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
                switch (exp_node.expression) {
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
                    else => {},
                }
            }
        }
    };

    pub const ReturnStatement = struct {
        token: Token,
        return_value: ?ExpressionNode = null,

        pub fn toString(self: *const ReturnStatement, writer: *std.Io.Writer) !void {
            try writer.print("{s} ", .{
                self.token.token_type.toString(),
            });

            if (self.return_value) |*value| {
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
        value: ?ExpressionNode = null,

        pub fn toString(self: *const LetStatement, writer: *std.Io.Writer) !void {
            try writer.print("{s} ", .{self.token.token_type.toString()});

            try self.name.toString(writer);

            _ = try writer.write(" = ");

            if (self.value) |*value| {
                try value.toString(writer);
            } else {
                _ = try writer.write("");
            }
            _ = try writer.write(";");
            try writer.flush();
        }
    };
};

pub const StatementNode = struct {
    statement: StatementType,

    pub fn init(statementType: StatementType) StatementNode {
        return .{ .statement = statementType };
    }

    pub fn toString(self: *const StatementNode, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.statement) {
            inline else => |*val| try val.toString(writer),
        }
    }
};

pub const ExpressionNode = struct {
    expression: ExpressionType,

    pub fn initIfExpression(tkn: Token, condition: *StatementType.ExpressionStatement, consequence: StatementType.BlockStatement, alternative: ?StatementType.BlockStatement) ExpressionNode {
        return .{
            .expression = .{
                .if_expression = ExpressionType.IfExpression.init(tkn, condition, consequence, alternative),
            },
        };
    }
    pub fn initIdentifier(value: []const u8) ExpressionNode {
        return .{
            .expression = .{
                .identifier = Identifier.init(value),
            },
        };
    }

    pub fn initIntegerLiteral(tkn: Token, val: u64) ExpressionNode {
        return .{
            .expression = .{
                .integer_literal = ExpressionType.IntegerLiteral.init(tkn, val),
            },
        };
    }

    pub fn initPrefixExpression(tkn: Token, right: *StatementType.ExpressionStatement) ExpressionNode {
        return .{
            .expression = .{
                .prefix_expression = ExpressionType.PrefixExpression.init(tkn, right),
            },
        };
    }

    pub fn initInfixExpression(tkn: Token, left: *StatementType.ExpressionStatement, right: *StatementType.ExpressionStatement) ExpressionNode {
        return .{
            .expression = .{
                .infix_expression = ExpressionType.InfixExpression.init(tkn, left, right),
            },
        };
    }

    pub fn initBooleanLiteral(tkn: Token, val: bool) ExpressionNode {
        return .{
            .expression = .{
                .boolean_literal = ExpressionType.BooleanLiteral.init(tkn, val),
            },
        };
    }

    pub fn toString(self: *const ExpressionNode, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.expression) {
            inline else => |*val| try val.toString(writer),
        }
    }
};

pub const Program = struct {
    statements: ArrayList(StatementNode),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{ .allocator = allocator, .statements = .empty };
    }

    pub fn toString(self: *const Program, writer: *std.Io.Writer) !void {
        for (self.statements.items) |*stmt| {
            try stmt.*.toString(writer);
        }
    }

    pub fn addStatement(self: *Program, statement: StatementNode) !void {
        try self.statements.append(self.allocator, statement);
    }

    pub fn deinit(self: *Program) void {
        for (self.statements.items) |*stmt| {
            switch (stmt.statement) {
                .expression => |*exp_stmt| exp_stmt.deinit(self.allocator),
                else => {},
            }
        }
        self.statements.deinit(self.allocator);
    }
};
