const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Token = @import("./token.zig").Token;
const TokenType = @import("./token.zig").TokenType;
const Parser = @import("./parser.zig").Parser;

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

fn testBooleanLiteral(val: *const ast.ExpressionType, literal_value: bool) !void {
    switch (val.*) {
        .boolean_literal => |exp| {
            try testing.expectEqual(literal_value, exp.value);
        },
        else => {
            std.debug.print("Expected a boolean literal. Got something else", .{});
            return error.TestUnexpectedResult;
        },
    }
}

const ExpectedValue = union(enum) {
    integer: u64,
    string: []const u8,
    boolean: bool,
};

fn testIdentifier(val: *const ast.ExpressionType, literal_value: []const u8) !void {
    switch (val.*) {
        .identifier => |*exp| {
            try testing.expectEqualStrings(literal_value, exp.*.value);
        },
        else => {
            std.debug.print("Expected identifier. Got something else", .{});
            return error.TestUnexpectedResult;
        },
    }
}

fn testLiteralExpression(exp: *const ast.ExpressionType, expected: ExpectedValue) !void {
    switch (expected) {
        .integer => |val| try testIntegerLiteral(exp, val),
        .string => |val| try testIdentifier(exp, val),
        .boolean => |val| try testBooleanLiteral(exp, val),
    }
}

fn testInfixExpression(exp: *const ast.ExpressionType, left: ExpectedValue, operator: []const u8, right: ExpectedValue) !void {
    switch (exp.*) {
        .infix_expression => |infix_exp| {
            if (infix_exp.left.expression) |*left_exp| {
                try testLiteralExpression(left_exp, left);
            } else {
                return error.TestUnexpectedResult;
            }
            try testing.expectEqualStrings(operator, infix_exp.operator);
            if (infix_exp.right.expression) |*right_exp| {
                try testLiteralExpression(right_exp, right);
            } else {
                return error.TestUnexpectedResult;
            }
        },
        else => {
            std.debug.print("Expected infix expression. Got something else", .{});
            return error.TestUnexpectedResult;
        },
    }
}

fn checkParserErrors(parser: *Parser) !void {
    testing.expectEqual(0, parser.allErrors().len) catch |err| {
        const errors = parser.allErrors();
        for (errors) |e| {
            std.debug.print("Found Error: {s}\n", .{e});
        }
        return err;
    };
}

test "print let statement" {
    var output_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output_writer.deinit();

    const statement: ast.StatementType = .{
        .let = .{
            .token = Token.init(.let, "let"),
            .name = ast.Identifier.init(Token.init(.iden, "myVar"), "myVar"),
            .value = ast.ExpressionType.initIdentifier(Token.init(.iden, "myValue"), "myValue"),
        },
    };

    const let_statement = statement.let;
    const node = ast.Node.implBy(&let_statement);

    try node.toString(&output_writer.writer);

    try testing.expectEqualStrings("let myVar = myValue;", output_writer.written());
}

test "print return statement" {
    var output_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output_writer.deinit();

    const statement: ast.StatementType = .{
        .@"return" = .{
            .token = Token.init(.@"return", "return"),
            .return_value = ast.ExpressionType.initIdentifier(Token.init(.iden, "myValue"), "myValue"),
        },
    };

    const return_statement: ast.StatementType.ReturnStatement = statement.@"return";
    const node = ast.Node.implBy(&return_statement);

    try node.toString(&output_writer.writer);

    try testing.expectEqualStrings("return myValue;", output_writer.written());
}

test "print expression statement" {
    var output_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer output_writer.deinit();

    const statement: ast.StatementType = .{
        .expression = .{
            .expression = ast.ExpressionType.initIdentifier(Token.init(.iden, "myValue"), "myValue"),
        },
    };

    const expression_statement = statement.expression;
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
        switch (stmt) {
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

    const expression_statement = switch (program.statements.items[0]) {
        .expression => |value| value,
        else => unreachable,
    };

    try testIdentifier(&expression_statement.expression.?, "foobar");
}

test "basic int parsing" {
    const input = "5;";

    const allocator = std.testing.allocator;

    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();
    const program = try parser.parse();

    try testing.expect(program.statements.items.len == 1);

    const expression_statement = switch (program.statements.items[0]) {
        .expression => |value| value,
        else => unreachable,
    };

    try testIntegerLiteral(&expression_statement.expression.?, 5);
}

test "basic boolean parsing" {
    const cases = [_]struct {
        input: []const u8,
        expected: bool,
    }{ .{
        .input = "true;",
        .expected = true,
    }, .{
        .input = "false;",
        .expected = false,
    } };

    for (cases) |case| {
        const allocator = std.testing.allocator;

        var lexer = Lexer.init(case.input);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();
        const program = try parser.parse();

        try checkParserErrors(&parser);

        try testing.expect(program.statements.items.len == 1);

        const expression_statement = switch (program.statements.items[0]) {
            .expression => |value| value,
            else => unreachable,
        };

        try testBooleanLiteral(&expression_statement.expression.?, case.expected);
    }
}

test "parse prefix expressions" {
    const cases = [_]struct {
        input: []const u8,
        operator: []const u8,
        value: ExpectedValue,
    }{
        .{
            .input = "!5",
            .operator = "!",
            .value = .{
                .integer = 5,
            },
        },
        .{
            .input = "-15",
            .operator = "-",
            .value = .{
                .integer = 15,
            },
        },
        .{
            .input = "!true;",
            .operator = "!",
            .value = .{
                .boolean = true,
            },
        },
        .{
            .input = "!false;",
            .operator = "!",
            .value = .{
                .boolean = false,
            },
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

        switch (program.statements.items[0]) {
            .expression => {},
            else => {
                std.debug.print("Expected an expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        }

        const parsed_statement = program.statements.items[0].expression;

        switch (parsed_statement.expression.?) {
            .prefix_expression => {},
            else => {
                std.debug.print("Expected a prefix expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        }

        const prefix_expression = parsed_statement.expression.?.prefix_expression;
        const right_expression = prefix_expression.right.expression.?;

        try testing.expectEqualStrings(case.operator, prefix_expression.operator);
        try testLiteralExpression(&right_expression, case.value);
    }
}

test "infix operator precedence" {
    const cases = [_]struct {
        input: []const u8,
        expected: []const u8,
        expected_items: usize = 1,
    }{
        .{
            .input = "-a * b",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e - f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        .{
            .input = "3 + 4; -5 * 5",
            .expected = "(3 + 4)((-5) * 5)",
            .expected_items = 2,
        },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "true",
            .expected = "true",
        },
        .{
            .input = "false",
            .expected = "false",
        },
        .{
            .input = "3 > 5 == false",
            .expected = "((3 > 5) == false)",
        },
        .{
            .input = "3 < 5 == true",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "1 + (2 + 3) + 4",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "(5 + 5) * 2",
            .expected = "((5 + 5) * 2)",
        },
        .{
            .input = "2 / (5 + 5)",
            .expected = "(2 / (5 + 5))",
        },
        .{
            .input = "-(5 + 5)",
            .expected = "(-(5 + 5))",
        },
        .{
            .input = "!(true == true)",
            .expected = "(!(true == true))",
        },
    };

    for (cases) |case| {
        const allocator = testing.allocator;

        var output_writer = std.Io.Writer.Allocating.init(std.testing.allocator);
        defer output_writer.deinit();

        var lexer = Lexer.init(case.input);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parse();

        try checkParserErrors(&parser);

        try testing.expectEqual(case.expected_items, program.*.statements.items.len);

        switch (program.statements.items[0]) {
            .expression => {},
            else => {
                std.debug.print("Expected an expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        }

        try program.toString(&output_writer.writer);

        try testing.expectEqualStrings(case.expected, output_writer.written());
    }
}

test "parse infix expressions" {
    const cases = [_]struct {
        input: []const u8,
        leftValue: ExpectedValue,
        operator: []const u8,
        rightValue: ExpectedValue,
    }{
        .{
            .input = "5 + 5;",
            .leftValue = .{ .integer = 5 },
            .operator = "+",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "5 - 5;",
            .leftValue = .{ .integer = 5 },
            .operator = "-",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "5 * 5;",
            .leftValue = .{ .integer = 5 },
            .operator = "*",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "5 / 5;",
            .leftValue = .{ .integer = 5 },
            .operator = "/",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "5 > 5;",
            .leftValue = .{ .integer = 5 },
            .operator = ">",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "5 < 5;",
            .leftValue = .{ .integer = 5 },
            .operator = "<",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "5 == 5;",
            .leftValue = .{ .integer = 5 },
            .operator = "==",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "5 != 5;",
            .leftValue = .{ .integer = 5 },
            .operator = "!=",
            .rightValue = .{ .integer = 5 },
        },
        .{
            .input = "true == true;",
            .leftValue = .{ .boolean = true },
            .operator = "==",
            .rightValue = .{ .boolean = true },
        },
        .{
            .input = "true != false;",
            .leftValue = .{ .boolean = true },
            .operator = "!=",
            .rightValue = .{ .boolean = false },
        },
        .{
            .input = "false == false;",
            .leftValue = .{ .boolean = false },
            .operator = "==",
            .rightValue = .{ .boolean = false },
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

        const statement = switch (program.statements.items[0]) {
            .expression => |*val| val,
            else => {
                std.debug.print("Expected an expression. Got something else.", .{});
                return error.TestUnexpectedResult;
            },
        };

        try testInfixExpression(&statement.expression.?, case.leftValue, case.operator, case.rightValue);
    }
}

test "parsing if expression" {
    const input = "if (x < y) { x }";

    const allocator = std.testing.allocator;
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parse();

    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.*.statements.items.len);

    const expression = switch (program.statements.items[0]) {
        .expression => |val| val,
        else => {
            std.debug.print("Expected an expression. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    const if_expression = switch (expression.expression.?) {
        .if_expression => |val| val,
        else => {
            std.debug.print("Expected if expression. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testInfixExpression(&if_expression.condition.expression.?, .{ .string = "x" }, "<", .{ .string = "y" });

    try testing.expectEqual(1, if_expression.consequence.statements.items.len);

    const consequence_expression = switch (if_expression.consequence.statements.items[0]) {
        .expression => |val| val,
        else => {
            std.debug.print("Expected an expression in consequence. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testIdentifier(&consequence_expression.expression.?, "x");

    try testing.expect(null == if_expression.alternative);
}

test "parsing if else expression" {
    const input = "if (x < y) { x } else { y }";

    const allocator = std.testing.allocator;
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parse();

    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.*.statements.items.len);

    const expression = switch (program.statements.items[0]) {
        .expression => |val| val,
        else => {
            std.debug.print("Expected an expression. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    const if_expression = switch (expression.expression.?) {
        .if_expression => |val| val,
        else => {
            std.debug.print("Expected if expression. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testInfixExpression(&if_expression.condition.expression.?, .{ .string = "x" }, "<", .{ .string = "y" });

    try testing.expectEqual(1, if_expression.consequence.statements.items.len);

    const consequence_expression = switch (if_expression.consequence.statements.items[0]) {
        .expression => |val| val,
        else => {
            std.debug.print("Expected an expression in consequence. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testIdentifier(&consequence_expression.expression.?, "x");

    const alternative = if_expression.alternative.?;

    try testing.expectEqual(1, alternative.statements.items.len);

    const alternative_expression = switch (alternative.statements.items[0]) {
        .expression => |val| val,
        else => {
            std.debug.print("Expected an expression in alternative. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testIdentifier(&alternative_expression.expression.?, "y");
}

test "paring function literals" {
    const input = "fn(x,y){ x + y; }";

    const allocator = std.testing.allocator;
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();

    const program = try parser.parse();

    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.*.statements.items.len);

    const expression = switch (program.statements.items[0]) {
        .expression => |val| val,
        else => {
            std.debug.print("Expected an expression. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    const fn_expression = switch (expression.expression.?) {
        .function_literal => |val| val,
        else => {
            std.debug.print("Expected if expression. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testing.expectEqual(2, fn_expression.parameters.len);

    try testing.expectEqualStrings("x", fn_expression.parameters[0].value);
    try testing.expectEqualStrings("y", fn_expression.parameters[1].value);

    try testing.expectEqual(1, fn_expression.body.statements.items.len);

    const body_expression = switch (fn_expression.body.statements.items[0]) {
        .expression => |val| val,
        else => {
            std.debug.print("Expected an expression in function body. Got something else.", .{});
            return error.TestUnexpectedResult;
        },
    };

    try testInfixExpression(&body_expression.expression.?, .{ .string = "x" }, "+", .{ .string = "y" });
}

test "function parameter parsing" {
    const cases = [_]struct {
        input: []const u8,
        expected_params: []const []const u8,
    }{
        .{ .input = "fn() {};", .expected_params = &.{} },
        .{ .input = "fn(x) {};", .expected_params = &.{"x"} },
        .{ .input = "fn(x, y, z) {};", .expected_params = &.{ "x", "y", "z" } },
    };

    for (cases) |case| {
        const allocator = std.testing.allocator;
        var lexer = Lexer.init(case.input);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parse();
        try checkParserErrors(&parser);

        try testing.expectEqual(1, program.statements.items.len);

        const expression = switch (program.statements.items[0]) {
            .expression => |val| val,
            else => return error.TestUnexpectedResult,
        };

        const fn_expression = switch (expression.expression.?) {
            .function_literal => |val| val,
            else => return error.TestUnexpectedResult,
        };

        try testing.expectEqual(case.expected_params.len, fn_expression.parameters.len);

        for (case.expected_params, 0..) |expected_param, i| {
            try testing.expectEqualStrings(expected_param, fn_expression.parameters[i].value);
        }
    }
}
