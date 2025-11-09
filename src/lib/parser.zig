const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;

const Precedence = enum(u8) {
    Lowest = 1,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
};

const ParsingFn = struct {
    ptr: *anyopaque,
    interface: *const Interface,

    pub const Interface = struct {
        prefixParseFn: *const fn (self: *anyopaque) ?ast.ExpressionNode,
        infixParseFn: *const fn (self: *anyopaque, left: ast.ExpressionNode) ?ast.ExpressionNode,
    };

    pub fn implBy(impl_obj: anytype) ParsingFn {
        const delegator = ParsingFnDelegator(impl_obj);
        return .{
            .ptr = impl_obj,
            .interface = &.{
                .prefixParseFn = delegator.prefixParseFn,
                .infixParseFn = delegator.infixParseFn,
            },
        };
    }

    pub fn hasPrefixParseFn(self: *ParsingFn) bool {
        return self.interface.hasPrefixParseFn(self.ptr);
    }

    pub fn hasInfixParseFn(self: *ParsingFn) bool {
        return self.interface.hasInfixParseFn(self.ptr);
    }

    pub fn prefixParseFn(self: *ParsingFn) ?ast.ExpressionNode {
        return self.interface.prefixParseFn(self.ptr);
    }

    pub fn infixParseFn(self: *ParsingFn, left: ast.ExpressionNode) ?ast.ExpressionNode {
        return self.interface.infixParseFn(self.ptr, left);
    }
};

inline fn ParsingFnDelegator(comptime ImplType: type) type {
    const ImplTypeInfo = @typeInfo(ImplType);
    return struct {
        fn hasFunction(comptime T: type, comptime name: []const u8) bool {
            if (!@hasDecl(T, name)) return false;

            const decl = @field(T, name);
            const type_info = @typeInfo(@TypeOf(decl));

            return type_info == .Fn;
        }

        fn hasPrefixParseFn() bool {
            hasFunction(ImplTypeInfo, "prefixParseFn");
        }

        fn hasInfixParseFn() bool {
            hasFunction(ImplTypeInfo, "infixParseFn");
        }

        fn prefixParseFn(self: *anyopaque) ?ast.ExpressionNode {
            if (!hasPrefixParseFn()) return null;

            const impl: ImplTypeInfo = @ptrCast(@alignCast(self));
            impl.prefixParseFn();
        }

        fn infixParseFn(self: *anyopaque, left: ast.ExpressionNode) ?ast.ExpressionNode {
            if (!hasPrefixParseFn()) return null;

            const impl: ImplTypeInfo = @ptrCast(@alignCast(self));
            impl.infixParseFn(left);
        }
    };
}

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token,
    peek_token: Token,
    program: ?*const ast.Program = null,
    errors: std.ArrayList([]const u8) = .empty,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) Self {
        const token1 = lexer.nextToken();
        const token2 = lexer.nextToken();

        return .{ .lexer = lexer, .current_token = token1, .peek_token = token2, .allocator = allocator };
    }

    pub fn parse(self: *Self) !*const ast.Program {
        const program = try self.allocator.create(ast.Program);
        program.* = ast.Program.init(self.allocator);
        sw: switch (self.current_token.token_type) {
            TokenType.eof => break :sw,
            else => {
                const statement = try self.parseStatement();
                if (statement) |stmt| {
                    try program.addStatement(stmt);
                }
                self.nextToken();
                continue :sw self.current_token.token_type;
            },
        }

        self.program = program;
        return self.program.?;
    }

    fn nextToken(self: *Self) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn parseStatement(self: *Self) !?ast.StatementNode {
        return switch (self.current_token.token_type) {
            TokenType.let => try self.parseLetStatement(),
            TokenType.@"return" => try self.parseReturnStatement(),
            else => null,
        };
    }

    fn parseReturnStatement(self: *Self) !?ast.StatementNode {
        const current_token = self.current_token;

        self.nextToken();

        while (!self.curTokenIs(TokenType.semicolon)) {
            self.nextToken();
        }

        return ast.StatementNode.init(ast.StatementType{
            .@"return" = .{
                .token = current_token,
            },
        });
    }

    fn parseLetStatement(self: *Self) !?ast.StatementNode {
        const current_token = self.current_token;

        if (!try self.expectPeek(TokenType.iden)) {
            return null;
        }

        const iden_token = self.current_token;

        if (!try self.expectPeek(TokenType.assign)) {
            return null;
        }

        while (!self.curTokenIs(TokenType.semicolon)) {
            self.nextToken();
        }

        return ast.StatementNode.init(ast.StatementType{ .let = .{
            .token = current_token,
            .name = .{
                .token_type = TokenType.iden,
                .value = iden_token.ch,
            },
        } });
    }

    fn curTokenIs(self: *Self, tokenType: TokenType) bool {
        return self.current_token.token_type == tokenType;
    }

    fn peekTokenIs(self: *Self, tokenType: TokenType) bool {
        return self.peek_token.token_type == tokenType;
    }

    fn expectPeek(self: *Self, tokenType: TokenType) !bool {
        if (self.peek_token.token_type == tokenType) {
            self.nextToken();
            return true;
        }

        try self.peekError(tokenType);
        return false;
    }

    fn peekError(self: *Self, tokenType: TokenType) !void {
        const msg = try std.fmt.allocPrint(self.allocator, "Expected next token to be {}, got {}", .{ tokenType, self.peek_token.token_type });
        try self.errors.append(self.allocator, msg);
    }

    pub fn allErrors(self: *Self) [][]const u8 {
        return self.errors.items;
    }

    pub fn deinit(self: *Self) void {
        if (self.program) |program| {
            const prog_mut = @constCast(program);
            prog_mut.deinit();
            self.allocator.destroy(prog_mut);
        }

        for (self.errors.items) |err_msg| {
            self.allocator.free(err_msg);
        }
        self.errors.deinit(self.allocator);
    }
};
