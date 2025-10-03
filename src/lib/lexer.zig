const std = @import("std");
const testing = std.testing;

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const LexerError = error{UnexpectedCharacter};

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    ch: ?u8 = null,

    const Self = @This();

    pub fn init(input: []const u8) Lexer {
        var lexer = Lexer{ .input = input };
        if (input.len > 0) {
            lexer.ch = input[0];
            lexer.read_position = 1;
        }
        return lexer;
    }

    fn readCharacter(self: *Self) void {
        if (self.read_position >= self.input.len) {
            self.ch = null;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn readIdentifier(self: *Self) ![]const u8 {
        const start = self.position;

        if (self.ch == null) return error.UnexpectedCharacter;

        while (Lexer.isLetter(self.ch.?)) {
            self.readCharacter();
            if (self.ch == null) return self.input[start..self.position];
        }
        return self.input[start..self.position];
    }

    fn readNumber(self: *Self) ![]const u8 {
        const start = self.position;

        if (self.ch == null) return error.UnexpectedCharacter;

        while (Lexer.isDigit(self.ch.?)) {
            self.readCharacter();
            if (self.ch == null) return self.input[start..self.position];
        }
        return self.input[start..self.position];
    }

    fn skipWhitespace(self: *Self) void {
        if (self.ch == null) return;

        while (Lexer.isWhitespace(self.ch.?)) {
            self.readCharacter();
            if (self.ch == null) return;
        }
    }

    fn peek(self: *Self) ?u8 {
        if (self.read_position >= self.input.len) return null;
        return self.input[self.read_position];
    }

    fn isWhitespace(ch: u8) bool {
        return std.ascii.isWhitespace(ch);
    }

    fn isLetter(ch: u8) bool {
        return std.ascii.isAlphabetic(ch) or ch == '_';
    }

    fn isDigit(ch: u8) bool {
        return std.ascii.isDigit(ch);
    }

    pub fn nextToken(self: *Self) Token {
        self.skipWhitespace();

        if (self.ch == null) {
            return Token.init(TokenType.eof, "eof");
        }

        const ch = self.ch.?;
        const ch_slice = self.input[self.position .. self.position + 1];

        if (Lexer.isLetter(ch)) {
            const nextTokenSlice = self.readIdentifier() catch @panic("Encountered an unexpected error while trying to read an identifier");

            const matching_keyword = Token.fromString(nextTokenSlice);
            if (matching_keyword) |match| {
                return match;
                // We did not match a keyword, so we assume it must be an identifier
            } else {
                return Token.init(TokenType.iden, nextTokenSlice);
            }
        } else if (Lexer.isDigit(ch)) {
            const nextTokenSlice = self.readNumber() catch @panic("Encountered an unexpected error while trying to read a number");
            return Token.init(TokenType.int, nextTokenSlice);
        } else {
            // Handle two-character operators
            if (ch == '=' or ch == '!') {
                if (self.peek() == '=') {
                    const operator_slice = self.input[self.position .. self.position + 2];
                    self.readCharacter();
                    self.readCharacter();
                    return Token.fromString(operator_slice) orelse Token.init(TokenType.illegal, operator_slice);
                }
            }

            // Handle single-character tokens
            const matching_token = Token.fromString(ch_slice);
            if (matching_token) |match| {
                self.readCharacter();
                return match;
            }

            // Illegal character
            self.readCharacter();
            return Token.init(TokenType.illegal, ch_slice);
        }
    }
};

test "read tokens" {
    const input =
        \\let five = 5;
        \\ let ten = 10;
        \\ let add = fn(x, y) {
        \\     x + y;
        \\ };
        \\ let result = add(five, ten);
        \\ !-/*5;
        \\ 5 < 10 > 5;
        \\ if (5 < 10) {
        \\     return true;
        \\ } else {
        \\     return false;
        \\ }
        \\ 10 == 10;
        \\ 10 != 9;
    ;

    const tests = [_]struct { TokenType, []const u8 }{
        .{ TokenType.let, "let" },
        .{ TokenType.iden, "five" },
        .{ TokenType.assign, "=" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.iden, "ten" },
        .{ TokenType.assign, "=" },
        .{ TokenType.int, "10" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.iden, "add" },
        .{ TokenType.assign, "=" },
        .{ TokenType.function, "fn" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.iden, "x" },
        .{ TokenType.comma, "," },
        .{ TokenType.iden, "y" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.iden, "x" },
        .{ TokenType.plus, "+" },
        .{ TokenType.iden, "y" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.iden, "result" },
        .{ TokenType.assign, "=" },
        .{ TokenType.iden, "add" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.iden, "five" },
        .{ TokenType.comma, "," },
        .{ TokenType.iden, "ten" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.bang, "!" },
        .{ TokenType.minus, "-" },
        .{ TokenType.slash, "/" },
        .{ TokenType.asterisk, "*" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.int, "5" },
        .{ TokenType.lt, "<" },
        .{ TokenType.int, "10" },
        .{ TokenType.gt, ">" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.@"if", "if" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.int, "5" },
        .{ TokenType.lt, "<" },
        .{ TokenType.int, "10" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.@"return", "return" },
        .{ TokenType.true, "true" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.@"else", "else" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.@"return", "return" },
        .{ TokenType.false, "false" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.int, "10" },
        .{ TokenType.eq, "==" },
        .{ TokenType.int, "10" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.int, "10" },
        .{ TokenType.not_eq, "!=" },
        .{ TokenType.int, "9" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.eof, "eof" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected_output| {
        const tkn = lexer.nextToken();
        try testing.expectEqual(expected_output[0], tkn.token_type);
        try testing.expectEqualStrings(expected_output[1], tkn.ch);
    }
}
