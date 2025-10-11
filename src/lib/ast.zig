const std = @import("std");
const ArrayList = std.ArrayList;
const SignlyLinkedList = @import("utils/SinglyLinkedList.zig");
const token = @import("./token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const Node = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        toString: fn (self: *anyopaque) []const u8,
    };
};

const ExpressionType = union(enum) {
    identifier: Identifier,
};

const Identifier = struct {
    token_type: TokenType,
    value: []const u8,
};

pub const StatementType = union(enum) {
    let: LetStatement,
    @"return": ReturnStatement,

    const ReturnStatement = struct {
        token: Token,
        return_value: ?ExpressionNode = null,
    };

    const LetStatement = struct {
        token: Token,
        name: Identifier,
    };
};

pub const StatementNode = struct {
    statement: StatementType,

    pub fn init(statementType: StatementType) StatementNode {
        return .{ .statement = statementType };
    }

    pub fn node(self: *StatementNode) Node {
        return .{ .ptr = self, .vtable = &.{
            .toString = toString,
        } };
    }

    pub fn toString(ctx: *anyopaque) []const u8 {
        const self: *StatementNode = @ptrCast(@alignCast(ctx));
        return self.token.toString();
    }
};

const ExpressionNode = struct {
    expression: ExpressionType,

    pub fn node(self: *ExpressionNode) Node {
        return .{ .ptr = self, .vtable = &.{
            .toString = toString,
        } };
    }

    pub fn toString(ctx: *anyopaque) []const u8 {
        const self: *ExpressionNode = @ptrCast(@alignCast(ctx));
        switch (self.expression) {
            .identifier => |val| {
                return val.token.toString();
            },
            else => unreachable,
        }
    }
};

pub const Program = struct {
    statements: ArrayList(StatementNode),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{ .allocator = allocator, .statements = .empty };
    }

    pub fn addStatement(self: *Program, statement: StatementNode) !void {
        try self.statements.append(self.allocator, statement);
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit(self.allocator);
    }

    pub fn toString(self: Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].toString();
        }
        return "";
    }
};
