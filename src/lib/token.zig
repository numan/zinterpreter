const std = @import("std");
const StaticStringMap = std.StaticStringMap;

pub const TokenType = enum {
    illegal,
    eof,
    // Identifiers + literals
    iden,
    int,
    //operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    not_eq,
    //Delimiters
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    // Keywords
    function,
    let,
    true,
    false,
    @"if",
    @"else",
    @"return",

    const StringTokenKVPair = struct { []const u8, TokenType };

    const mappings: []const StringTokenKVPair = &.{
        .{ "=", .assign },
        .{ ";", .semicolon },
        .{ ",", .comma },
        .{ "+", .plus },
        .{ "-", .minus },
        .{ "!", .bang },
        .{ "*", .asterisk },
        .{ "/", .slash },
        .{ "<", .lt },
        .{ ">", .gt },
        .{ "==", .eq },
        .{ "!=", .not_eq },
        .{ "(", .lparen },
        .{ ")", .rparen },
        .{ "{", .lbrace },
        .{ "}", .rbrace },
        .{ "fn", .function },
        .{ "let", .let },
        .{ "true", .true },
        .{ "false", .false },
        .{ "if", .@"if" },
        .{ "else", .@"else" },
        .{ "return", .@"return" },
    };

    const TMapType = StaticStringMap(TokenType);
    const map = TMapType.initComptime(TokenType.mappings);

    pub fn toString(self: TokenType) []const u8 {
        inline for (mappings) |kv| {
            if (kv[1] == self) return kv[0];
        }
        unreachable;
    }

    pub fn fromString(ch: []const u8) ?TokenType {
        return map.get(ch);
    }
};

pub const Token = struct {
    token_type: TokenType,
    ch: []const u8,

    pub fn init(token_type: TokenType, ch: []const u8) Token {
        return .{
            .token_type = token_type,
            .ch = ch,
        };
    }

    pub fn fromString(ch: []const u8) ?Token {
        if (TokenType.fromString(ch)) |token_type| {
            return Token.init(token_type, ch);
        }
        return null;
    }
};
