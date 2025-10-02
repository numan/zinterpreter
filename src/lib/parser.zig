const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const ast = @import("./ast.zig");

const std = @import("std");
const testing = std.testing;

pub const Parser = struct {
    lexer: *Lexer,
    current_token: Token,
    peek_token: Token,
    statements: std.ArrayList(ast.StatementNode),
    program: ?*const ast.Program = null,
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) Self {
        const token1 = lexer.nextToken();
        const token2 = lexer.nextToken();

        return .{ .lexer = lexer, .current_token = token1, .peek_token = token2, .statements = .empty, .allocator = allocator };
    }

    pub fn parse(self: *Self) !*const ast.Program {
        const program = try self.allocator.create(ast.Program);
        program.* = ast.Program.init(self.allocator);
        sw: switch (self.current_token.token_type) {
            TokenType.eof => break :sw,
            else => {
                const statement = self.parseStatement();
                if (statement) |stmt| {
                    try program.statements.append(self.allocator, stmt);
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

    fn parseStatement(self: *Self) ?ast.StatementNode {
        return switch (self.current_token.token_type) {
            TokenType.let => self.parseLetStatement(),
            else => null,
        };
    }

    fn parseLetStatement(self: *Self) ?ast.StatementNode {
        const current_token = self.current_token;

        if (!self.expectPeek(TokenType.iden)) {
            return null;
        }

        const iden_token = self.current_token;

        sw: switch (self.curTokenIs(TokenType.semicolon)) {
            true => {
                self.nextToken();
                continue :sw self.curTokenIs(TokenType.semicolon);
            },
            else => {},
        }

        return ast.StatementNode.init(ast.StatementType{ .let = .{ .token = current_token, .name = .{ .token = TokenType.iden, .value = iden_token.ch } } });
    }

    fn curTokenIs(self: *Self, tokenType: TokenType) bool {
        return self.current_token.token_type == tokenType;
    }

    fn peekTokenIs(self: *Self, tokenType: TokenType) bool {
        return self.peek_token.token_type == tokenType;
    }

    fn expectPeek(self: *Self, tokenType: TokenType) bool {
        if (self.peek_token.token_type == tokenType) {
            self.nextToken();
            return true;
        }

        return false;
    }
    pub fn deinit(self: *Self) void {
        if (self.program) |program| {
            const prog_mut = @constCast(program);
            prog_mut.deinit();
            self.allocator.destroy(prog_mut);
        }
    }
};

test "parser" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    const expected_ast = [_]struct { TokenType, []const u8 }{
        .{ TokenType.iden, "x" },
        .{ TokenType.iden, "y" },
        .{ TokenType.iden, "foobar" },
    };

    const allocator = std.testing.allocator;

    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();
    const program = try parser.parse();

    for (expected_ast, 0..) |expected_output, i| {
        const stmt = program.statements.items[i];
        switch (stmt.statement) {
            .let => |let_stmt| {
                try testing.expectEqualStrings(expected_output[1], let_stmt.name.value);
            },
        }
    }
}
