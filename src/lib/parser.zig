const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const Precedence = @import("./parsing_utils.zig").Precedence;

pub const ParseError = error{ InvalidCharacter, Overflow, OutOfMemory };

const PrefixParseFn = *const fn (*Parser) ParseError!?ast.StatementType.ExpressionStatement;
const InfixParseFn = *const fn (ast.ExpressionNode, *Parser) ParseError!?ast.StatementType.ExpressionStatement;

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token,
    peek_token: Token,
    program: ?*ast.Program = null,
    errors: std.ArrayList([]const u8) = .empty,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) Self {
        const token1 = lexer.nextToken();
        const token2 = lexer.nextToken();
        return .{
            .lexer = lexer,
            .current_token = token1,
            .peek_token = token2,
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Self) ParseError!*const ast.Program {
        const program = try self.allocator.create(ast.Program);
        program.* = ast.Program.init(self.allocator);

        while (self.current_token.token_type != .eof) {
            if (try self.parseStatement()) |stmt| {
                try program.addStatement(stmt);
            }
            self.nextToken();
        }

        self.program = program;
        return self.program.?;
    }

    fn nextToken(self: *Self) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn parseStatement(self: *Self) ParseError!?ast.StatementNode {
        return switch (self.current_token.token_type) {
            TokenType.let => try self.parseLetStatement(),
            TokenType.@"return" => try self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseExpressionStatement(self: *Self) ParseError!?ast.StatementNode {
        const expression = self.parseExpression(.lowest);
        if (self.peek_token.token_type == .semicolon) {
            self.nextToken();
        }

        return expression;
    }

    fn parseExpression(self: *Self, precedence: Precedence) ParseError!?ast.StatementNode {
        _ = precedence;
        if (self.getPrefixParseFn(self.current_token.token_type)) |prefixFn| {
            const expression = try prefixFn(self) orelse unreachable;

            return ast.StatementNode.init(.{
                .expression = expression,
            });
        } else {
            try self.noPrefixParseFnError(self.current_token.token_type);
            return null;
        }
    }

    fn parseSubExpression(self: *Self, precedence: Precedence) ParseError!?ast.StatementType.ExpressionStatement {
        const node = try self.parseExpression(precedence) orelse return null;
        return node.statement.expression;
    }

    fn parseReturnStatement(self: *Self) ParseError!?ast.StatementNode {
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

    fn parseLetStatement(self: *Self) ParseError!?ast.StatementNode {
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

    fn expectPeek(self: *Self, token_type: TokenType) ParseError!bool {
        if (self.peek_token.token_type == token_type) {
            self.nextToken();
            return true;
        }

        try self.peekError(token_type);
        return false;
    }

    fn peekError(self: *Self, token_type: TokenType) ParseError!void {
        const msg = try std.fmt.allocPrint(self.allocator, "Expected next token to be {}, got {}", .{ token_type, self.peek_token.token_type });
        try self.errors.append(self.allocator, msg);
    }

    fn noPrefixParseFnError(self: *Self, token_type: TokenType) ParseError!void {
        const msg = try std.fmt.allocPrint(self.allocator, "No prefix parse fn found for token type {}", .{
            token_type,
        });
        try self.errors.append(self.allocator, msg);
    }

    pub fn allErrors(self: *Self) [][]const u8 {
        return self.errors.items;
    }

    fn parseIdentifier(parser: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        return ast.StatementType.ExpressionStatement.initIdentifierExpression(parser.current_token.ch);
    }

    fn parseIntegerLiteral(parser: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        const val = try std.fmt.parseInt(u64, parser.current_token.ch, 10);
        return ast.StatementType.ExpressionStatement.initIntegerLiteralExpression(parser.current_token, val);
    }

    fn parsePrefixOperator(parser: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        const cur_token = parser.current_token;
        parser.nextToken();
        const right = try parser.parseSubExpression(.prefix) orelse unreachable;
        const right_ptr = try parser.allocator.create(ast.StatementType.ExpressionStatement);
        right_ptr.* = right;
        return ast.StatementType.ExpressionStatement.initPrefixExpression(cur_token, right_ptr);
    }

    inline fn getPrefixParseFn(self: *const Self, token_type: TokenType) ?PrefixParseFn {
        _ = self;
        return switch (token_type) {
            .iden => Self.parseIdentifier,
            .int => Self.parseIntegerLiteral,
            .bang, .minus => Self.parsePrefixOperator,
            else => null,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.program) |program| {
            program.*.deinit();
            self.allocator.destroy(program);
        }

        for (self.errors.items) |err_msg| {
            self.allocator.free(err_msg);
        }
        self.errors.deinit(self.allocator);
    }
};
