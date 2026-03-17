const std = @import("std");
const testing = std.testing;

const compiler = @import("./compiler.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Parser = @import("./parser.zig").Parser;
const Object = @import("./object.zig").Object;
const vm_mod = @import("./vm.zig");
const Vm = vm_mod.Vm;
const SymbolTable = @import("./symbol_table.zig").SymbolTable;

const Compiler = compiler.Compiler;

const HashEntry = struct {
    key: Object.HashKey,
    value: i64,
};

const Expected = union(enum) {
    int: i64,
    boolean: bool,
    string: []const u8,
    int_array: []const i64,
    int_hash: []const HashEntry,
    err: []const u8,
    null,
};

const VmTestCase = struct {
    input: []const u8,
    expected: Expected,
};

fn parse(allocator: std.mem.Allocator, input: []const u8) !*@import("./ast.zig").Program {
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    return try parser.parse();
}

fn testIntegerObject(expected: i64, obj: Object) !void {
    const integer = switch (obj) {
        .int => |value| value,
        else => {
            std.debug.print("object is not Integer. got={s}\n", .{obj.typeName()});
            return error.TestUnexpectedResult;
        },
    };
    try testing.expectEqual(expected, integer.value);
}

fn testBooleanObject(expected: bool, obj: Object) !void {
    const boolean = switch (obj) {
        .bool => |value| value,
        else => {
            std.debug.print("object is not Boolean. got={s}\n", .{obj.typeName()});
            return error.TestUnexpectedResult;
        },
    };
    try testing.expectEqual(expected, boolean.value);
}

fn testNullObject(obj: Object) !void {
    if (obj != .null) {
        std.debug.print("object is not Null. got={s}\n", .{obj.typeName()});
        return error.TestUnexpectedResult;
    }
}

fn testStringObject(expected: []const u8, obj: Object) !void {
    const str = switch (obj) {
        .string => |value| value,
        else => {
            std.debug.print("object is not String. got={s}\n", .{obj.typeName()});
            return error.TestUnexpectedResult;
        },
    };
    try testing.expectEqualStrings(expected, str.value);
}

fn testErrorObject(expected_msg: []const u8, obj: Object) !void {
    const err_obj = switch (obj) {
        .err => |e| e,
        else => {
            std.debug.print("object is not Error. got={s}\n", .{obj.typeName()});
            return error.TestUnexpectedResult;
        },
    };
    try testing.expectEqualStrings(expected_msg, err_obj.msg);
}

fn testExpectedObject(expected: Expected, obj: Object) !void {
    switch (expected) {
        .int => |val| try testIntegerObject(val, obj),
        .boolean => |val| try testBooleanObject(val, obj),
        .string => |val| try testStringObject(val, obj),
        .err => |val| try testErrorObject(val, obj),
        .int_array => |expected_elements| {
            const array = switch (obj) {
                .array => |a| a,
                else => {
                    std.debug.print("object is not Array. got={s}\n", .{obj.typeName()});
                    return error.TestUnexpectedResult;
                },
            };
            try testing.expectEqual(expected_elements.len, array.elements.len);
            for (expected_elements, array.elements) |exp, elem| {
                try testIntegerObject(exp, elem);
            }
        },
        .int_hash => |expected_entries| {
            const hash = switch (obj) {
                .hash => |h| h,
                else => {
                    std.debug.print("object is not Hash. got={s}\n", .{obj.typeName()});
                    return error.TestUnexpectedResult;
                },
            };
            try testing.expectEqual(expected_entries.len, hash.pairs.count());
            for (expected_entries) |entry| {
                const pair = hash.pairs.get(entry.key) orelse {
                    std.debug.print("no pair for given key in hash\n", .{});
                    return error.TestUnexpectedResult;
                };
                try testIntegerObject(entry.value, pair.value);
            }
        },
        .null => try testNullObject(obj),
    }
}

fn runVmTests(tests: []const VmTestCase) !void {
    const allocator = testing.allocator;

    for (tests) |tt| {
        var parser = blk: {
            var lexer = Lexer.init(tt.input);
            break :blk Parser.init(allocator, &lexer);
        };
        defer parser.deinit();
        const program = try parser.parse();

        var symbol_table = SymbolTable.init(allocator, null);
        defer symbol_table.deinit();
        var constants = std.ArrayList(Object).empty;
        defer constants.deinit(allocator);
        var comp = Compiler.init(allocator, &symbol_table, &constants);
        defer comp.deinit();
        try comp.enterScope();
        try comp.compile(program);

        var vm_arena = std.heap.ArenaAllocator.init(allocator);
        defer vm_arena.deinit();
        var globals: [vm_mod.globals_size]Object = undefined;
        var test_writer = std.Io.Writer.Allocating.init(allocator);
        defer test_writer.deinit();
        var vm = try Vm.init(comp.bytecode(), &globals, vm_arena.allocator(), &test_writer.writer);
        try vm.run();

        const stack_elem = vm.lastPoppedStackElem() orelse {
            std.debug.print("stackTop returned null\n", .{});
            return error.TestUnexpectedResult;
        };

        try testExpectedObject(tt.expected, stack_elem);
    }
}

test "integer arithmetic" {
    const tests = [_]VmTestCase{
        .{ .input = "1", .expected = .{ .int = 1 } },
        .{ .input = "2", .expected = .{ .int = 2 } },
        .{ .input = "1 + 2", .expected = .{ .int = 3 } },
        .{ .input = "1 - 2", .expected = .{ .int = -1 } },
        .{ .input = "1 * 2", .expected = .{ .int = 2 } },
        .{ .input = "4 / 2", .expected = .{ .int = 2 } },
        .{ .input = "50 / 2 * 2 + 10 - 5", .expected = .{ .int = 55 } },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = .{ .int = 10 } },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = .{ .int = 32 } },
        .{ .input = "5 * 2 + 10", .expected = .{ .int = 20 } },
        .{ .input = "5 + 2 * 10", .expected = .{ .int = 25 } },
        .{ .input = "5 * (2 + 10)", .expected = .{ .int = 60 } },
    };

    try runVmTests(&tests);
}

test "boolean expressions" {
    const tests = [_]VmTestCase{
        .{ .input = "true", .expected = .{ .boolean = true } },
        .{ .input = "false", .expected = .{ .boolean = false } },
        .{ .input = "1 < 2", .expected = .{ .boolean = true } },
        .{ .input = "1 > 2", .expected = .{ .boolean = false } },
        .{ .input = "1 < 1", .expected = .{ .boolean = false } },
        .{ .input = "1 > 1", .expected = .{ .boolean = false } },
        .{ .input = "1 == 1", .expected = .{ .boolean = true } },
        .{ .input = "1 != 1", .expected = .{ .boolean = false } },
        .{ .input = "1 == 2", .expected = .{ .boolean = false } },
        .{ .input = "1 != 2", .expected = .{ .boolean = true } },
        .{ .input = "true == true", .expected = .{ .boolean = true } },
        .{ .input = "false == false", .expected = .{ .boolean = true } },
        .{ .input = "true == false", .expected = .{ .boolean = false } },
        .{ .input = "true != false", .expected = .{ .boolean = true } },
        .{ .input = "false != true", .expected = .{ .boolean = true } },
        .{ .input = "(1 < 2) == true", .expected = .{ .boolean = true } },
        .{ .input = "(1 < 2) == false", .expected = .{ .boolean = false } },
        .{ .input = "(1 > 2) == true", .expected = .{ .boolean = false } },
        .{ .input = "(1 > 2) == false", .expected = .{ .boolean = true } },
    };

    try runVmTests(&tests);
}

test "prefix expressions" {
    const tests = [_]VmTestCase{
        .{ .input = "-5", .expected = .{ .int = -5 } },
        .{ .input = "-10", .expected = .{ .int = -10 } },
        .{ .input = "-50 + 100 + -50", .expected = .{ .int = 0 } },
        .{ .input = "!true", .expected = .{ .boolean = false } },
        .{ .input = "!false", .expected = .{ .boolean = true } },
        .{ .input = "!5", .expected = .{ .boolean = false } },
        .{ .input = "!!true", .expected = .{ .boolean = true } },
        .{ .input = "!!false", .expected = .{ .boolean = false } },
        .{ .input = "!!5", .expected = .{ .boolean = true } },
    };

    try runVmTests(&tests);
}

test "conditionals" {
    const tests = [_]VmTestCase{
        .{ .input = "if (true) { 10 }", .expected = .{ .int = 10 } },
        .{ .input = "if (true) { 10 } else { 20 }", .expected = .{ .int = 10 } },
        .{ .input = "if (false) { 10 } else { 20 }", .expected = .{ .int = 20 } },
        .{ .input = "if (1) { 10 }", .expected = .{ .int = 10 } },
        .{ .input = "if (1 < 2) { 10 }", .expected = .{ .int = 10 } },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = .{ .int = 10 } },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = .{ .int = 20 } },
        .{ .input = "if (1 > 2) { 10 }", .expected = .null },
        .{ .input = "if (false) { 10 }", .expected = .null },
        .{ .input = "if ((if (false) { 10 })) { 10 } else { 20 }", .expected = .{ .int = 20 } },
    };

    try runVmTests(&tests);
}

test "let statements" {
    const tests = [_]VmTestCase{
        .{ .input = "let one = 1; one;", .expected = .{ .int = 1 } },
        .{ .input = "let one = 1; let two = 2; one + two;", .expected = .{ .int = 3 } },
        .{ .input = "let one = 1; let two = one; two;", .expected = .{ .int = 1 } },
        .{ .input = "let x = 5; x;", .expected = .{ .int = 5 } },
        .{ .input = "let x = 5; let y = 10; x + y;", .expected = .{ .int = 15 } },
    };

    try runVmTests(&tests);
}

test "string expressions" {
    const tests = [_]VmTestCase{
        .{ .input = "\"monkey\"", .expected = .{ .string = "monkey" } },
        .{ .input = "\"mon\" + \"key\"", .expected = .{ .string = "monkey" } },
        .{ .input = "\"mon\" + \"key\" + \"banana\"", .expected = .{ .string = "monkeybanana" } },
    };

    try runVmTests(&tests);
}

test "array literals" {
    const tests = [_]VmTestCase{
        .{ .input = "[]", .expected = .{ .int_array = &.{} } },
        .{ .input = "[1, 2, 3]", .expected = .{ .int_array = &.{ 1, 2, 3 } } },
        .{ .input = "[1 + 2, 3 * 4, 5 + 6]", .expected = .{ .int_array = &.{ 3, 12, 11 } } },
    };

    try runVmTests(&tests);
}

test "hash literals" {
    const tests = [_]VmTestCase{
        .{ .input = "{}", .expected = .{ .int_hash = &.{} } },
        .{
            .input = "{1: 2, 2: 3}",
            .expected = .{ .int_hash = &.{
                .{ .key = (Object.Integer.init(1)).hashKey(), .value = 2 },
                .{ .key = (Object.Integer.init(2)).hashKey(), .value = 3 },
            } },
        },
        .{
            .input = "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
            .expected = .{ .int_hash = &.{
                .{ .key = (Object.Integer.init(2)).hashKey(), .value = 4 },
                .{ .key = (Object.Integer.init(6)).hashKey(), .value = 16 },
            } },
        },
    };

    try runVmTests(&tests);
}

test "index expressions" {
    const tests = [_]VmTestCase{
        .{ .input = "[1, 2, 3][1]", .expected = .{ .int = 2 } },
        .{ .input = "[1, 2, 3][0 + 2]", .expected = .{ .int = 3 } },
        .{ .input = "[[1, 1, 1]][0][0]", .expected = .{ .int = 1 } },
        .{ .input = "[][0]", .expected = .null },
        .{ .input = "[1, 2, 3][99]", .expected = .null },
        .{ .input = "[1][-1]", .expected = .null },
        .{ .input = "{1: 1, 2: 2}[1]", .expected = .{ .int = 1 } },
        .{ .input = "{1: 1, 2: 2}[2]", .expected = .{ .int = 2 } },
        .{ .input = "{1: 1}[0]", .expected = .null },
        .{ .input = "{}[0]", .expected = .null },
    };

    try runVmTests(&tests);
}

test "calling functions without arguments" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let fivePlusTen = fn() { 5 + 10; };
                \\fivePlusTen();
            ,
            .expected = .{ .int = 15 },
        },
        .{
            .input =
                \\let one = fn() { 1; };
                \\let two = fn() { 2; };
                \\one() + two();
            ,
            .expected = .{ .int = 3 },
        },
        .{
            .input =
                \\let a = fn() { 1; };
                \\let b = fn() { a() + 1; };
                \\let c = fn() { b() + 1; };
                \\c();
            ,
            .expected = .{ .int = 3 },
        },
    };

    try runVmTests(&tests);
}

test "calling functions with return statement" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let earlyExit = fn() { return 99; 100; };
                \\earlyExit();
            ,
            .expected = .{ .int = 99 },
        },
        .{
            .input =
                \\let earlyExit = fn() { return 99; return 100; };
                \\earlyExit();
            ,
            .expected = .{ .int = 99 },
        },
    };

    try runVmTests(&tests);
}

test "calling functions without return value" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let noReturn = fn() { };
                \\noReturn();
            ,
            .expected = .null,
        },
        .{
            .input =
                \\let noReturn = fn() { };
                \\let noReturnTwo = fn() { noReturn(); };
                \\noReturn();
                \\noReturnTwo();
            ,
            .expected = .null,
        },
    };

    try runVmTests(&tests);
}

test "calling functions with bindings" {
    const tests = [_]VmTestCase{
        .{
            .input = "let one = fn() { let one = 1; one }; one();",
            .expected = .{ .int = 1 },
        },
        .{
            .input =
                \\let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                \\oneAndTwo();
            ,
            .expected = .{ .int = 3 },
        },
        .{
            .input =
                \\let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                \\let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
                \\oneAndTwo() + threeAndFour();
            ,
            .expected = .{ .int = 10 },
        },
        .{
            .input =
                \\let firstFoobar = fn() { let foobar = 50; foobar; };
                \\let secondFoobar = fn() { let foobar = 100; foobar; };
                \\firstFoobar() + secondFoobar();
            ,
            .expected = .{ .int = 150 },
        },
        .{
            .input =
                \\let globalSeed = 50;
                \\let minusOne = fn() { let num = 1; globalSeed - num; };
                \\let minusTwo = fn() { let num = 2; globalSeed - num; };
                \\minusOne() + minusTwo();
            ,
            .expected = .{ .int = 97 },
        },
    };

    try runVmTests(&tests);
}

test "first class functions" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let returnsOne = fn() { 1; };
                \\let returnsOneReturner = fn() { returnsOne; };
                \\returnsOneReturner()();
            ,
            .expected = .{ .int = 1 },
        },
        .{
            .input = "let returnsOneReturner = fn() { let returnsOne = fn() { 1; }; returnsOne; }; returnsOneReturner()();",
            .expected = .{ .int = 1 },
        },
    };

    try runVmTests(&tests);
}

test "calling functions with arguments and bindings" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let identity = fn(a) { a; };
                \\identity(4);
            ,
            .expected = .{ .int = 4 },
        },
        .{
            .input =
                \\let sum = fn(a, b) { a + b; };
                \\sum(1, 2);
            ,
            .expected = .{ .int = 3 },
        },
        .{
            .input =
                \\let sum = fn(a, b) {
                \\    let c = a + b;
                \\    c;
                \\};
                \\sum(1, 2);
            ,
            .expected = .{ .int = 3 },
        },
        .{
            .input =
                \\let sum = fn(a, b) {
                \\    let c = a + b;
                \\    c;
                \\};
                \\sum(1, 2) + sum(3, 4);
            ,
            .expected = .{ .int = 10 },
        },
        .{
            .input =
                \\let sum = fn(a, b) {
                \\    let c = a + b;
                \\    c;
                \\};
                \\let outer = fn() {
                \\    sum(1, 2) + sum(3, 4);
                \\};
                \\outer();
            ,
            .expected = .{ .int = 10 },
        },
        .{
            .input =
                \\let globalNum = 10;
                \\let sum = fn(a, b) {
                \\    let c = a + b;
                \\    c + globalNum;
                \\};
                \\let outer = fn() {
                \\    sum(1, 2) + sum(3, 4) + globalNum;
                \\};
                \\outer() + globalNum;
            ,
            .expected = .{ .int = 50 },
        },
    };

    try runVmTests(&tests);
}

const VmErrorTestCase = struct {
    input: []const u8,
    expected_error: anyerror,
};

fn runVmErrorTests(tests: []const VmErrorTestCase) !void {
    const allocator = testing.allocator;

    for (tests) |tt| {
        var parser = blk: {
            var lexer = Lexer.init(tt.input);
            break :blk Parser.init(allocator, &lexer);
        };
        defer parser.deinit();
        const program = try parser.parse();

        var symbol_table = SymbolTable.init(allocator, null);
        defer symbol_table.deinit();
        var constants = std.ArrayList(Object).empty;
        defer constants.deinit(allocator);
        var comp = Compiler.init(allocator, &symbol_table, &constants);
        defer comp.deinit();
        try comp.enterScope();
        try comp.compile(program);

        var vm_arena = std.heap.ArenaAllocator.init(allocator);
        defer vm_arena.deinit();
        var globals: [vm_mod.globals_size]Object = undefined;
        var test_writer = std.Io.Writer.Allocating.init(allocator);
        defer test_writer.deinit();
        var vm = try Vm.init(comp.bytecode(), &globals, vm_arena.allocator(), &test_writer.writer);
        try testing.expectError(tt.expected_error, vm.run());
    }
}

test "calling functions with wrong arguments" {
    const tests = [_]VmErrorTestCase{
        .{ .input = "fn() { 1; }(1);", .expected_error = error.WrongArgumentCount },
        .{ .input = "fn(a) { a; }();", .expected_error = error.WrongArgumentCount },
        .{ .input = "fn(a, b) { a + b; }(1);", .expected_error = error.WrongArgumentCount },
    };
    try runVmErrorTests(&tests);
}

test "builtin functions" {
    const tests = [_]VmTestCase{
        .{ .input = "len(\"\")", .expected = .{ .int = 0 } },
        .{ .input = "len(\"four\")", .expected = .{ .int = 4 } },
        .{ .input = "len(\"hello world\")", .expected = .{ .int = 11 } },
        .{ .input = "len(1)", .expected = .{ .err = "argument to `len` not supported, got int" } },
        .{ .input = "len(\"one\", \"two\")", .expected = .{ .err = "wrong number of arguments. got=2, want=1" } },
        .{ .input = "len([1, 2, 3])", .expected = .{ .int = 3 } },
        .{ .input = "len([])", .expected = .{ .int = 0 } },
        .{ .input = "puts(\"hello\", \"world!\")", .expected = .null },
        .{ .input = "first([1, 2, 3])", .expected = .{ .int = 1 } },
        .{ .input = "first([])", .expected = .null },
        .{ .input = "first(1)", .expected = .{ .err = "argument to `first` must be ARRAY, got int" } },
        .{ .input = "last([1, 2, 3])", .expected = .{ .int = 3 } },
        .{ .input = "last([])", .expected = .null },
        .{ .input = "last(1)", .expected = .{ .err = "argument to `last` must be ARRAY, got int" } },
        .{ .input = "rest([1, 2, 3])", .expected = .{ .int_array = &.{ 2, 3 } } },
        .{ .input = "rest([])", .expected = .null },
        .{ .input = "push([], 1)", .expected = .{ .int_array = &.{1} } },
        .{ .input = "push(1, 1)", .expected = .{ .err = "argument to `push` must be ARRAY, got int" } },
    };

    try runVmTests(&tests);
}

test "closures" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let newClosure = fn(a) {
                \\    fn() { a; };
                \\};
                \\let closure = newClosure(99);
                \\closure();
            ,
            .expected = .{ .int = 99 },
        },
        .{
            .input =
                \\let newAdder = fn(a, b) {
                \\    fn(c) { a + b + c };
                \\};
                \\let adder = newAdder(1, 2);
                \\adder(8);
            ,
            .expected = .{ .int = 11 },
        },
        .{
            .input =
                \\let newAdder = fn(a, b) {
                \\    let c = a + b;
                \\    fn(d) { c + d };
                \\};
                \\let adder = newAdder(1, 2);
                \\adder(8);
            ,
            .expected = .{ .int = 11 },
        },
        .{
            .input =
                \\let newAdderOuter = fn(a, b) {
                \\    let c = a + b;
                \\    fn(d) {
                \\        let e = d + c;
                \\        fn(f) { e + f; };
                \\    };
                \\};
                \\let newAdderInner = newAdderOuter(1, 2);
                \\let adder = newAdderInner(3);
                \\adder(8);
            ,
            .expected = .{ .int = 14 },
        },
        .{
            .input =
                \\let a = 1;
                \\let newAdderOuter = fn(b) {
                \\    fn(c) {
                \\        fn(d) { a + b + c + d };
                \\    };
                \\};
                \\let newAdderInner = newAdderOuter(2);
                \\let adder = newAdderInner(3);
                \\adder(8);
            ,
            .expected = .{ .int = 14 },
        },
        .{
            .input =
                \\let newClosure = fn(a, b) {
                \\    let one = fn() { a; };
                \\    let two = fn() { b; };
                \\    fn() { one() + two(); };
                \\};
                \\let closure = newClosure(9, 90);
                \\closure();
            ,
            .expected = .{ .int = 99 },
        },
    };

    try runVmTests(&tests);
}

test "recursive functions" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let countDown = fn(x) {
                \\    if (x == 0) {
                \\        return 0;
                \\    } else {
                \\        countDown(x - 1);
                \\    }
                \\};
                \\countDown(1);
            ,
            .expected = .{ .int = 0 },
        },
        .{
            .input =
                \\let countDown = fn(x) {
                \\    if (x == 0) {
                \\        return 0;
                \\    } else {
                \\        countDown(x - 1);
                \\    }
                \\};
                \\let wrapper = fn() {
                \\    countDown(1);
                \\};
                \\wrapper();
            ,
            .expected = .{ .int = 0 },
        },
        .{
            .input =
                \\let wrapper = fn() {
                \\    let countDown = fn(x) {
                \\        if (x == 0) {
                \\            return 0;
                \\        } else {
                \\            countDown(x - 1);
                \\        }
                \\    };
                \\    countDown(1);
                \\};
                \\wrapper();
            ,
            .expected = .{ .int = 0 },
        },
    };

    try runVmTests(&tests);
}

test "recursive fibonacci" {
    const tests = [_]VmTestCase{
        .{
            .input =
                \\let fibonacci = fn(x) {
                \\    if (x == 0) {
                \\        return 0;
                \\    } else {
                \\        if (x == 1) {
                \\            return 1;
                \\        } else {
                \\            fibonacci(x - 1) + fibonacci(x - 2);
                \\        }
                \\    }
                \\};
                \\fibonacci(15);
            ,
            .expected = .{ .int = 610 },
        },
    };

    try runVmTests(&tests);
}
