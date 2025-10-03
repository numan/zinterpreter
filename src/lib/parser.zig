const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;

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
            else => null,
        };
    }

    fn parseLetStatement(self: *Self) !?ast.StatementNode {
        const current_token = self.current_token;

        if (!try self.expectPeek(TokenType.iden)) {
            return null;
        }

        const iden_token = self.current_token;

        while (!self.curTokenIs(TokenType.semicolon)) {
            self.nextToken();
        }

        return ast.StatementNode.init(ast.StatementType{ .let = .{ .token = current_token, .name = .{ .token_type = TokenType.iden, .value = iden_token.ch } } });
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

test "parse error" {
    const input =
        \\let ;x 
    ;

    var lexer = Lexer.init(input);
    var parser = Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    _ = try parser.parse();

    try testing.expectEqual(1, parser.allErrors().len);
    try testing.expectEqualStrings("Expected next token to be .iden, got .semicolon", parser.allErrors()[0]);
}

test "parser" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    const expected_ast = [_]struct { TokenType, []const u8 }{
        .{ TokenType.let, "x" },
        .{ TokenType.let, "y" },
        .{ TokenType.let, "foobar" },
    };

    const allocator = std.testing.allocator;

    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();
    const program = try parser.parse();

    for (expected_ast, 0..) |expected_output, i| {
        const stmt = program.statements.items[i];
        const expected_token_type, const expected_name = expected_output;
        switch (stmt.statement) {
            .let => |let_stmt| {
                try testing.expectEqualStrings(expected_name, let_stmt.name.value);
                try testing.expectEqual(expected_token_type, let_stmt.token.token_type);
            },
        }
    }
}
