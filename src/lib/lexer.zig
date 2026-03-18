const std = @import("std");
const testing = std.testing;

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    ch: ?u8 = null,

    pub fn init(input: []const u8) Lexer {
        var lexer = Lexer{ .input = input };
        if (input.len > 0) {
            lexer.ch = input[0];
            lexer.read_position = 1;
        }
        return lexer;
    }

    fn readCharacter(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = null;
        } else {
            self.ch = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn readIdentifier(self: *Lexer) Token {
        const start = self.position;

        while (self.ch) |ch| {
            if (!isLetter(ch)) break;
            self.readCharacter();
        }

        const slice = self.input[start..self.position];
        return Token.fromString(slice) orelse Token.init(.iden, slice);
    }

    fn readNumber(self: *Lexer) Token {
        const start = self.position;

        while (self.ch) |c| {
            if (!std.ascii.isDigit(c)) break;
            self.readCharacter();
        }

        // Read a float
        if (self.ch) |next_character| {
            if (next_character == '.') {
                self.readCharacter();
                while (self.ch) |c| {
                    if (!std.ascii.isDigit(c)) break;
                    self.readCharacter();
                }
                return .init(.float, self.input[start..self.position]);
            }
        }
        return .init(.int, self.input[start..self.position]);
    }

    fn skipWhitespace(self: *Lexer) void {
        while (self.ch) |ch| {
            if (!std.ascii.isWhitespace(ch)) break;
            self.readCharacter();
        }
    }

    fn peek(self: *Lexer) ?u8 {
        if (self.read_position >= self.input.len) return null;
        return self.input[self.read_position];
    }

    fn isLetter(ch: u8) bool {
        return std.ascii.isAlphabetic(ch) or ch == '_';
    }

    fn readString(self: *Lexer) Token {
        self.readCharacter(); // Skip the opening quote
        const start = self.position;
        var escaped = false;

        while (self.ch) |ch| {
            if (escaped) {
                escaped = false;
            } else if (ch == '\\') {
                escaped = true;
            } else if (ch == '"') {
                const value = self.input[start..self.position];
                self.readCharacter(); // Skip the closing quote
                return Token.init(.string, value);
            }

            self.readCharacter();
        }

        return Token.init(.illegal, "Unterminated string literal");
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();

        const ch = self.ch orelse return Token.init(.eof, "eof");

        if (ch == '"') {
            return self.readString();
        } else if (isLetter(ch)) {
            return self.readIdentifier();
        } else if (std.ascii.isDigit(ch)) {
            return self.readNumber();
        } else {
            // Handle two-character operators
            if (ch == '=' or ch == '!') {
                if (self.peek() == '=') {
                    const operator_slice = self.input[self.position .. self.position + 2];
                    self.readCharacter();
                    self.readCharacter();
                    return Token.fromString(operator_slice) orelse Token.init(.illegal, operator_slice);
                }
            }

            // Handle single-character tokens
            const ch_slice = self.input[self.position .. self.position + 1];
            if (Token.fromString(ch_slice)) |match| {
                self.readCharacter();
                return match;
            }

            // Illegal character
            self.readCharacter();
            return Token.init(.illegal, ch_slice);
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
        \\ "foo bar"
        \\ "foobar"
        \\ [1, 2]
        \\ {"foo": "bar"}
        \\12;
        \\10.;
        \\10.3;
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
        .{ TokenType.string, "foo bar" },
        .{ TokenType.string, "foobar" },
        .{ TokenType.lbracket, "[" },
        .{ TokenType.int, "1" },
        .{ TokenType.comma, "," },
        .{ TokenType.int, "2" },
        .{ TokenType.rbracket, "]" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.string, "foo" },
        .{ TokenType.colon, ":" },
        .{ TokenType.string, "bar" },
        .{ TokenType.rbrace, "}" },
        .{ .int, "12" },
        .{ .semicolon, ";" },
        .{ .float, "10." },
        .{ .semicolon, ";" },
        .{ .float, "10.3" },
        .{ .semicolon, ";" },
        .{ TokenType.eof, "eof" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected_output| {
        const tkn = lexer.nextToken();
        try testing.expectEqual(expected_output[0], tkn.token_type);
        try testing.expectEqualStrings(expected_output[1], tkn.ch);
    }
}

test "read escaped string tokens" {
    const input =
        \\ "hello \"world\""
        \\ "hello\n world"
        \\ "hello\t\t\tworld"
    ;

    const tests = [_]struct { TokenType, []const u8 }{
        .{ TokenType.string, "hello \\\"world\\\"" },
        .{ TokenType.string, "hello\\n world" },
        .{ TokenType.string, "hello\\t\\t\\tworld" },
        .{ TokenType.eof, "eof" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected_output| {
        const tkn = lexer.nextToken();
        try testing.expectEqual(expected_output[0], tkn.token_type);
        try testing.expectEqualStrings(expected_output[1], tkn.ch);
    }
}
