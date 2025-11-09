const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const Parser = @import("./parser.zig").Parser;

test "print let statement" {
    var output_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output_writer.deinit();

    const statement_node: ast.StatementNode = .{ .statement = .{
        .let = .{
            .token = Token.init(.let, "let"),
            .name = ast.Identifier.init("myVar"),
            .value = ast.ExpressionNode.initIdentifier("myValue"),
        },
    } };

    const let_statement = statement_node.statement.let;
    var node = ast.Node.implBy(&let_statement);

    try node.toString(&output_writer.writer);

    try testing.expectEqualStrings("let myVar = myValue;", output_writer.written());
}

test "print return statement" {
    var output_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output_writer.deinit();

    const return_node: ast.StatementNode = .{ .statement = .{
        .@"return" = .{
            .token = Token.init(.@"return", "return"),
            .return_value = ast.ExpressionNode.initIdentifier("myValue"),
        },
    } };

    const return_statement: ast.StatementType.ReturnStatement = return_node.statement.@"return";
    var node = ast.Node.implBy(&return_statement);

    try node.toString(&output_writer.writer);

    try testing.expectEqualStrings("return myValue;", output_writer.written());
}

test "print expression statement" {
    var output_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output_writer.deinit();

    const expression_node: ast.StatementNode = .{
        .statement = .{
            .expression = .{
                .expression = ast.ExpressionNode.initIdentifier("myValue"),
            },
        },
    };

    const expression_statement = expression_node.statement.expression;
    var node = ast.Node.implBy(&expression_statement);
    try node.toString(&output_writer.writer);

    try testing.expectEqualStrings("myValue", output_writer.written());
}

test "parse return" {
    const input =
        \\return 5;
        \\return 10;
        \\return 838383;
    ;

    const expected_number_of_statements = 3;
    var lexer = Lexer.init(input);
    var parser = Parser.init(testing.allocator, &lexer);
    defer parser.deinit();
    const program = try parser.parse();

    try testing.expectEqual(expected_number_of_statements, program.statements.items.len);
}

test "parse let error" {
    const input =
        \\let x 5;
        \\let = 10;
        \\let 838383;
    ;

    const expected_errors = [_][]const u8{
        "Expected next token to be .assign, got .int",
        "Expected next token to be .iden, got .assign",
        "Expected next token to be .iden, got .int",
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    _ = try parser.parse();
    const errors = parser.allErrors();

    try testing.expectEqual(3, errors.len);
    for (expected_errors, 0..) |expected_error, i| {
        try testing.expectEqualStrings(expected_error, errors[i]);
    }
}

test "parser let" {
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
            else => unreachable,
        }
    }
}

test "basic identifier parsing" {
    const input = "foobar;";

    const allocator = std.testing.allocator;

    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();
    const program = try parser.parse();

    try testing.expect(program.statements.items.len == 1);

    const expression_statement = switch (program.statements.items[0].statement) {
        .expression => |value| value,
        else => unreachable,
    };

    const identifier = switch (expression_statement.expression.?.expression) {
        .identifier => |*value| value,
    };

    try testing.expectEqualStrings("foobar", identifier.value);
}
