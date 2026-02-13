const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const Precedence = @import("./parsing_utils.zig").Precedence;

pub const ParseError = error{ InvalidCharacter, Overflow, OutOfMemory };

const PrefixParseFn = *const fn (*Parser) ParseError!?ast.StatementType.ExpressionStatement;
const InfixParseFn = *const fn (*Parser, *ast.StatementType.ExpressionStatement) ParseError!?ast.StatementType.ExpressionStatement;
const IdentifierListType = std.ArrayList(ast.Identifier);

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

    fn parseStatement(self: *Self) ParseError!?ast.StatementType {
        return switch (self.current_token.token_type) {
            TokenType.let => try self.parseLetStatement(),
            TokenType.@"return" => try self.parseReturnStatement(),
            else => self.parseExpressionStatement(),
        };
    }

    fn parseExpressionStatement(self: *Self) ParseError!?ast.StatementType {
        const expression = try self.parseExpression(.lowest);
        if (self.peek_token.token_type == .semicolon) {
            self.nextToken();
        }

        if (expression) |expr| {
            return ast.StatementType{
                .expression = expr,
            };
        }

        return null;
    }

    fn parseExpression(self: *Self, precedence: Precedence) ParseError!?ast.StatementType.ExpressionStatement {
        if (self.getPrefixParseFn(self.current_token.token_type)) |prefix_fn| {
            var left = try prefix_fn(self) orelse return null;

            while (self.peek_token.token_type != .semicolon and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
                const infix_fn = self.getInixParseFn(self.peek_token.token_type) orelse return left;

                self.nextToken();

                const left_ptr = try self.allocator.create(ast.StatementType.ExpressionStatement);
                left_ptr.* = left;
                left = try infix_fn(self, left_ptr) orelse return left;
            }
            return left;
        } else {
            try self.noPrefixParseFnError(self.current_token.token_type);
            return null;
        }
    }

    fn parseSubExpression(self: *Self, precedence: Precedence) ParseError!?ast.StatementType.ExpressionStatement {
        return try self.parseExpression(precedence) orelse return null;
    }

    fn parseReturnStatement(self: *Self) ParseError!?ast.StatementType {
        const current_token = self.current_token;

        self.nextToken();

        const val = try self.parseExpression(.lowest);

        if (self.peek_token.token_type == .semicolon) {
            self.nextToken();
        }

        return ast.StatementType{
            .@"return" = .{
                .token = current_token,
                .value = val orelse .{ .expression = null },
            },
        };
    }

    fn parseLetStatement(self: *Self) ParseError!?ast.StatementType {
        const current_token = self.current_token;

        if (!try self.expectPeek(TokenType.iden)) {
            return null;
        }

        const iden_token = self.current_token;

        if (!try self.expectPeek(TokenType.assign)) {
            return null;
        }

        self.nextToken();

        const exp_value = try self.parseExpression(.lowest) orelse return null;

        if (self.peek_token.token_type == .semicolon) {
            self.nextToken();
        }

        return ast.StatementType{ .let = .{
            .token = current_token,
            .name = ast.Identifier.init(iden_token, iden_token.ch),
            .value = exp_value,
        } };
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
        return ast.StatementType.ExpressionStatement.initIdentifierExpression(parser.current_token, parser.current_token.ch);
    }

    fn parseIntegerLiteral(parser: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        const val = try std.fmt.parseInt(i64, parser.current_token.ch, 10);
        return ast.StatementType.ExpressionStatement.initIntegerLiteralExpression(parser.current_token, val);
    }

    fn parseBooleanLiteral(parser: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        const val = if (std.mem.eql(u8, parser.current_token.ch, "true")) true else false;
        return ast.StatementType.ExpressionStatement.initBooleanLiteralExpression(parser.current_token, val);
    }

    fn parsePrefixOperator(parser: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        const cur_token = parser.current_token;
        parser.nextToken();
        const right = try parser.parseSubExpression(.prefix) orelse unreachable;
        const right_ptr = try parser.allocator.create(ast.StatementType.ExpressionStatement);
        right_ptr.* = right;
        return ast.StatementType.ExpressionStatement.initPrefixExpression(cur_token, right_ptr);
    }

    fn parseInfixExpression(self: *Self, left: *ast.StatementType.ExpressionStatement) ParseError!?ast.StatementType.ExpressionStatement {
        const current_token = self.current_token;
        const precedence = self.curPrecedence();
        self.nextToken();
        const right = try self.parseExpression(precedence) orelse return null;
        const right_ptr = try self.allocator.create(ast.StatementType.ExpressionStatement);
        right_ptr.* = right;

        return ast.StatementType.ExpressionStatement.initInfixExpression(current_token, left, right_ptr);
    }

    fn parseGroupedExpression(self: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        self.nextToken();

        const exp = try self.parseExpression(.lowest) orelse return null;

        if (!try self.expectPeek(TokenType.rparen)) {
            return null;
        }

        return exp;
    }

    fn parseIfExpression(self: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        const current_token = self.current_token;
        if (!try self.expectPeek(.lparen)) return null;

        self.nextToken();

        const condition = try self.allocator.create(ast.StatementType.ExpressionStatement);
        const parsed_condition = try self.parseExpression(.lowest);

        condition.* = parsed_condition.?;

        if (!try self.expectPeek(.rparen)) return null;
        if (!try self.expectPeek(.lbrace)) return null;

        const consequence = try self.parseBlockStatement();

        var alternative: ?ast.StatementType.BlockStatement = null;
        if (self.peek_token.token_type == .@"else") {
            self.nextToken();

            if (!try self.expectPeek(.lbrace)) return null;

            alternative = try self.parseBlockStatement();
        }

        return ast.StatementType.ExpressionStatement.initIfExpression(current_token, condition, consequence, alternative);
    }

    fn parseBlockStatement(self: *Self) ParseError!ast.StatementType.BlockStatement {
        var block_stmt = ast.StatementType.BlockStatement.init(self.current_token, self.allocator);
        self.nextToken();

        while (self.current_token.token_type != .rbrace and self.current_token.token_type != .eof) {
            const stmt = try self.parseStatement();
            if (stmt) |val| {
                try block_stmt.addStatement(val);
            }
            self.nextToken();
        }
        return block_stmt;
    }

    fn parseFunctionLiteral(self: *Self) ParseError!?ast.StatementType.ExpressionStatement {
        const current_token = self.current_token;

        if (!try self.expectPeek(.lparen)) {
            return null;
        }

        const params = try self.parseFunctionParams() orelse return null;

        if (!try self.expectPeek(.lbrace)) {
            return null;
        }

        const body = try self.parseBlockStatement();

        return ast.StatementType.ExpressionStatement.initFunctionLiteral(
            current_token,
            params,
            body,
        );
    }

    fn parseFunctionParams(self: *Self) ParseError!?[]ast.Identifier {
        var list: IdentifierListType = .empty;

        if (self.peek_token.token_type == .rparen) {
            self.nextToken();
            return list.items;
        }

        self.nextToken();

        try list.append(
            self.allocator,
            ast.Identifier.init(self.current_token, self.current_token.ch),
        );

        while (self.peek_token.token_type == .comma) {
            self.nextToken();
            self.nextToken();
            try list.append(
                self.allocator,
                ast.Identifier.init(self.current_token, self.current_token.ch),
            );
        }

        if (!try self.expectPeek(.rparen)) {
            return null;
        }

        return try list.toOwnedSlice(self.allocator);
    }

    fn parseCallExpression(self: *Self, function: *ast.StatementType.ExpressionStatement) ParseError!?ast.StatementType.ExpressionStatement {
        const current_token = self.current_token;
        const arguments = try self.parseCallArguments() orelse return null;
        return ast.StatementType.ExpressionStatement.initCallExpression(current_token, function, arguments);
    }

    fn parseCallArguments(self: *Self) ParseError!?[]ast.StatementType.ExpressionStatement {
        var list = std.ArrayList(ast.StatementType.ExpressionStatement).empty;

        if (self.peek_token.token_type == .rparen) {
            self.nextToken();
            return list.items;
        }

        self.nextToken();

        const first_arg = try self.parseExpression(.lowest) orelse return null;
        try list.append(self.allocator, first_arg);

        while (self.peek_token.token_type == .comma) {
            self.nextToken();
            self.nextToken();
            const arg = try self.parseExpression(.lowest) orelse return null;
            try list.append(self.allocator, arg);
        }

        if (!try self.expectPeek(.rparen)) {
            return null;
        }

        return try list.toOwnedSlice(self.allocator);
    }

    pub fn getPrecedence(self: *const Self, token_type: TokenType) Precedence {
        _ = self;
        return switch (token_type) {
            .eq, .not_eq => .equals,
            .lt, .gt => .less_greater,
            .plus, .minus => .sum,
            .slash, .asterisk => .product,
            .lparen => .call,
            else => .lowest,
        };
    }

    pub fn peekPrecedence(self: *const Self) Precedence {
        return self.getPrecedence(self.peek_token.token_type);
    }

    pub fn curPrecedence(self: *const Self) Precedence {
        return self.getPrecedence(self.current_token.token_type);
    }

    inline fn getPrefixParseFn(self: *const Self, token_type: TokenType) ?PrefixParseFn {
        _ = self;
        return switch (token_type) {
            .iden => Self.parseIdentifier,
            .int => Self.parseIntegerLiteral,
            .lparen => Self.parseGroupedExpression,
            .@"if" => Self.parseIfExpression,
            .function => Self.parseFunctionLiteral,
            inline .true, .false => Self.parseBooleanLiteral,
            inline .bang, .minus => Self.parsePrefixOperator,
            else => null,
        };
    }

    inline fn getInixParseFn(self: *const Self, token_type: TokenType) ?InfixParseFn {
        _ = self;
        return switch (token_type) {
            inline .plus, .minus, .slash, .asterisk, .eq, .not_eq, .lt, .gt => Self.parseInfixExpression,
            .lparen => Self.parseCallExpression,
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
