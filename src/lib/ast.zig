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
        return .{ .ptr = impl_obj, .vtable = &.{
            .toString = delegator.toString,
            .tokenLiteral = delegator.tokenLiteral,
        } };
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
            try impl.*.tokenLiteral();
        }
    };
}

const ExpressionType = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,

    pub fn toString(self: *const ExpressionType, writer: *std.Io.Writer) !void {
        switch (self.*) {
            inline else => |*val| {
                try val.toString(writer);
            },
        }
    }

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

    pub const ExpressionStatement = struct {
        expression: ?ExpressionNode,

        const Self = @This();

        pub fn initIdentifierExpression(value: []const u8) Self {
            return .{
                .expression = ExpressionNode.initIdentifier(value),
            };
        }

        pub fn initIntegerLiteralExpression(tkn: Token, val: i64) Self {
            return .{
                .expression = ExpressionNode.initIntegerLiteral(tkn, val),
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

    pub fn initExpressionStatement(tkn: Token) StatementNode {
        return .{ .statement = StatementType.initExpressionStatement(tkn) };
    }
};

pub const ExpressionNode = struct {
    expression: ExpressionType,

    pub fn initIdentifier(value: []const u8) ExpressionNode {
        return .{ .expression = .{ .identifier = Identifier.init(value) } };
    }

    pub fn initIntegerLiteral(tkn: Token, val: i64) ExpressionNode {
        return .{ .expression = .{ .integer_literal = ExpressionType.IntegerLiteral.init(tkn, val) } };
    }

    pub fn toString(self: *const ExpressionNode, writer: *std.Io.Writer) !void {
        switch (self.expression) {
            else => |*val| {
                return @constCast(val).*.toString(writer);
            },
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
            writer.write("\n");
        }
    }

    pub fn addStatement(self: *Program, statement: StatementNode) !void {
        try self.statements.append(self.allocator, statement);
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit(self.allocator);
    }
};
