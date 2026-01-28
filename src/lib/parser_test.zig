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
    const node = ast.Node.implBy(&let_statement);

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
    const node = ast.Node.implBy(&return_statement);

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
    const node = ast.Node.implBy(&expression_statement);
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
        "No prefix parse fn found for token type .assign",
        "Expected next token to be .iden, got .int",
    };

    var lexer = Lexer.init(input);
    var parser = Parser.init(testing.allocator, &lexer);
    defer parser.deinit();

    _ = try parser.parse();
    const errors = parser.allErrors();

    try testing.expectEqual(4, errors.len);
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
        else => unreachable,
    };

    try testing.expectEqualStrings("foobar", identifier.value);
}

test "basic int parsing" {
    const input = "5;";

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

    const int_literal = switch (expression_statement.expression.?.expression) {
        .integer_literal => |*value| value,
        else => unreachable,
    };

    try testing.expectEqualStrings("5", int_literal.*.tokenLiteral());
}

fn testIntegerLiteral(val: *const ast.ExpressionType, literal_value: u64) !void {
    switch (val.*) {
        .integer_literal => |exp| {
            try testing.expectEqual(literal_value, exp.value);

            var buf: [21]u8 = undefined;
            const expected = try std.fmt.bufPrint(&buf, "{}", .{literal_value});
            const exp_node = ast.ExpNode.implBy(&exp);
            try testing.expectEqualStrings(expected, exp_node.tokenLiteral());
        },
        else => {
            std.debug.print("Expected an integer literal. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    }
}

fn checkParserErrors(parser: *Parser) !void {
    testing.expectEqual(0, parser.allErrors().len) catch |err| {
        const errors = parser.allErrors();
        for (errors) |e| {
            std.debug.print("{s}\n", .{e});
        }
        return err;
    };
}

test "parse prefix expressions" {
    const cases = [_]struct {
        input: []const u8,
        operator: []const u8,
        int_value: u64,
    }{
        .{
            .input = "!5",
            .operator = "!",
            .int_value = 5,
        },
        .{
            .input = "-15",
            .operator = "-",
            .int_value = 15,
        },
    };

    for (cases) |case| {
        const allocator = std.testing.allocator;
        var lexer = Lexer.init(case.input);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parse();

        try checkParserErrors(&parser);

        try testing.expectEqual(1, program.*.statements.items.len);

        switch (program.statements.items[0].statement) {
            .expression => {},
            else => {
                std.debug.print("Expected an expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        }

        const parsed_statement = program.statements.items[0].statement.expression;

        switch (parsed_statement.expression.?.expression) {
            .prefix_expression => {},
            else => {
                std.debug.print("Expected a prefix expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        }

        const prefix_expression = parsed_statement.expression.?.expression.prefix_expression;
        const right_expression = prefix_expression.right.expression.?.expression;

        try testing.expectEqualStrings(case.operator, prefix_expression.operator);
        try testIntegerLiteral(&right_expression, case.int_value);
    }
}

test "parse infix expressions" {
    const cases = [_]struct {
        input: []const u8,
        leftValue: u64,
        operator: []const u8,
        rightValue: u64,
    }{
        .{
            .input = "5 + 5;",
            .leftValue = 5,
            .operator = "+",
            .rightValue = 5,
        },
        .{
            .input = "5 - 5;",
            .leftValue = 5,
            .operator = "-",
            .rightValue = 5,
        },
        .{
            .input = "5 * 5;",
            .leftValue = 5,
            .operator = "*",
            .rightValue = 5,
        },
        .{
            .input = "5 / 5;",
            .leftValue = 5,
            .operator = "/",
            .rightValue = 5,
        },
        .{
            .input = "5 > 5;",
            .leftValue = 5,
            .operator = ">",
            .rightValue = 5,
        },
        .{
            .input = "5 < 5;",
            .leftValue = 5,
            .operator = "<",
            .rightValue = 5,
        },
        .{
            .input = "5 == 5;",
            .leftValue = 5,
            .operator = "==",
            .rightValue = 5,
        },
        .{
            .input = "5 != 5;",
            .leftValue = 5,
            .operator = "!=",
            .rightValue = 5,
        },
    };

    for (cases) |case| {
        const allocator = std.testing.allocator;
        var lexer = Lexer.init(case.input);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parse();

        try checkParserErrors(&parser);

        try testing.expectEqual(1, program.*.statements.items.len);

        const statement = switch (program.statements.items[0].statement) {
            .expression => |*val| val,
            else => {
                std.debug.print("Expected an expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        };

        const infix_expression = switch (statement.expression.?.expression) {
            .infix_expression => |*val| val,
            else => {
                std.debug.print("Expected an infix expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        };

        try testIntegerLiteral(&infix_expression.left.expression.?.expression, case.leftValue);
        try testing.expectEqualStrings(case.operator, infix_expression.operator);
        try testIntegerLiteral(&infix_expression.right.expression.?.expression, case.rightValue);
    }
}
