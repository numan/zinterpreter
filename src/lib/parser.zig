const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const Precedence = @import("./parsing_utils.zig").Precedence;

const ParsingFn = struct {
    ptr: *const anyopaque,
    interface: *const Interface,

    pub const Interface = struct {
        prefixParseFn: *const fn (self: *const anyopaque, parser: *const Parser) ?ast.StatementType.ExpressionStatement,
        infixParseFn: *const fn (self: *const anyopaque, left: ast.ExpressionNode, parser: *const Parser) ?ast.StatementType.ExpressionStatement,
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

    pub fn hasPrefixParseFn(self: *const ParsingFn) bool {
        return self.interface.hasPrefixParseFn(self.ptr);
    }

    pub fn hasInfixParseFn(self: *const ParsingFn) bool {
        return self.interface.hasInfixParseFn(self.ptr);
    }

    pub fn prefixParseFn(self: *const ParsingFn, parser: *const Parser) ?ast.StatementType.ExpressionStatement {
        return self.interface.prefixParseFn(self.ptr, parser);
    }

    pub fn infixParseFn(self: *const ParsingFn, left: ast.ExpressionNode, parser: *const Parser) ?ast.StatementType.ExpressionStatement {
        return self.interface.infixParseFn(self.ptr, left, parser);
    }
};

inline fn ParsingFnDelegator(impl_obj: anytype) type {
    const ImplType = @TypeOf(impl_obj);
    const type_info = @typeInfo(ImplType);
    const T = if (type_info == .pointer) type_info.pointer.child else ImplType;
    return struct {
        fn hasFunction(comptime name: []const u8) bool {
            if (!@hasDecl(T, name)) return false;

            const decl = @field(T, name);
            const decl_type_info = @typeInfo(@TypeOf(decl));

            return decl_type_info == .@"fn";
        }

        fn hasPrefixParseFn() bool {
            return hasFunction("prefixParseFn");
        }

        fn hasInfixParseFn() bool {
            return hasFunction("infixParseFn");
        }

        fn prefixParseFn(self: *const anyopaque, parser: *const Parser) ?ast.StatementType.ExpressionStatement {
            if (comptime !hasPrefixParseFn()) return null;

            const impl: ImplType = @ptrCast(@alignCast(self));
            return impl.*.prefixParseFn(parser);
        }

        fn infixParseFn(self: *const anyopaque, left: ast.ExpressionNode, parser: *const Parser) ?ast.StatementType.ExpressionStatement {
            if (comptime !hasInfixParseFn()) return null;

            const impl: ImplType = @ptrCast(@alignCast(self));
            return impl.*.infixParseFn(left, parser);
        }
    };
}
const IdentifierParser = struct {
    const Self = @This();
    fn prefixParseFn(self: *const Self, parser: *const Parser) ast.StatementType.ExpressionStatement {
        _ = self;
        return ast.StatementType.ExpressionStatement.initIdentifierExpression(parser.current_token.ch);
    }
}{};

const ParsingFnMap = std.AutoHashMap(TokenType, ParsingFn);

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token,
    peek_token: Token,
    program: ?*const ast.Program = null,
    errors: std.ArrayList([]const u8) = .empty,
    allocator: std.mem.Allocator,
    parsing_fns: ParsingFnMap,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) !Self {
        const token1 = lexer.nextToken();
        const token2 = lexer.nextToken();

        return .{
            .lexer = lexer,
            .current_token = token1,
            .peek_token = token2,
            .allocator = allocator,
            .parsing_fns = blk: {
                var map = ParsingFnMap.init(allocator);
                try map.put(TokenType.iden, ParsingFn.implBy(&IdentifierParser));
                break :blk map;
            },
        };
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
            else => self.parseExpressionStatement(),
        };
    }

    fn parseExpressionStatement(self: *Self) !?ast.StatementNode {
        const expression = self.parseExpression(.Lowest);
        if (self.peek_token.token_type == .semicolon) {
            self.nextToken();
        }

        return expression;
    }

    fn parseExpression(self: *Self, precedence: Precedence) ?ast.StatementNode {
        _ = precedence;
        const parsingFns = self.parsing_fns.get(self.current_token.token_type) orelse return null;
        const expression = parsingFns.prefixParseFn(self) orelse return null;

        return ast.StatementNode.init(.{
            .expression = expression,
        });
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
        self.parsing_fns.deinit();
    }
};
