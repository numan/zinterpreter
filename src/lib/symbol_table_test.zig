const std = @import("std");
const testing = std.testing;
const SymbolTable = @import("symbol_table.zig").SymbolTable;
const Symbol = @import("symbol_table.zig").Symbol;
const SymbolScope = @import("symbol_table.zig").SymbolScope;

fn expectSymbolEqual(expected: Symbol, actual: Symbol) !void {
    try testing.expectEqualStrings(expected.name, actual.name);
    try testing.expectEqual(expected.scope, actual.scope);
    try testing.expectEqual(expected.index, actual.index);
}

test "define" {
    const expected_global = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
    };

    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();

    const a = try global.define("a");
    try expectSymbolEqual(expected_global[0], a);

    const b = try global.define("b");
    try expectSymbolEqual(expected_global[1], b);

    var local = SymbolTable.initEnclosed(testing.allocator, &global);
    defer local.deinit();

    const c = try local.define("c");
    try expectSymbolEqual(.{ .name = "c", .scope = .local, .index = 0 }, c);

    const d = try local.define("d");
    try expectSymbolEqual(.{ .name = "d", .scope = .local, .index = 1 }, d);
}

test "resolve global" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();

    _ = try global.define("a");
    _ = try global.define("b");

    const expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
    };

    for (expected) |sym| {
        const result = global.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try expectSymbolEqual(sym, result);
    }
}

test "resolve local" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();
    _ = try global.define("a");
    _ = try global.define("b");

    var local = SymbolTable.initEnclosed(testing.allocator, &global);
    defer local.deinit();
    _ = try local.define("c");
    _ = try local.define("d");

    const expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
        .{ .name = "c", .scope = .local, .index = 0 },
        .{ .name = "d", .scope = .local, .index = 1 },
    };

    for (expected) |sym| {
        const result = local.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try expectSymbolEqual(sym, result);
    }
}

test "resolve nested local" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();
    _ = try global.define("a");
    _ = try global.define("b");

    var first_local = SymbolTable.initEnclosed(testing.allocator, &global);
    defer first_local.deinit();
    _ = try first_local.define("c");
    _ = try first_local.define("d");

    var second_local = SymbolTable.initEnclosed(testing.allocator, &first_local);
    defer second_local.deinit();
    _ = try second_local.define("e");
    _ = try second_local.define("f");

    const first_expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
        .{ .name = "c", .scope = .local, .index = 0 },
        .{ .name = "d", .scope = .local, .index = 1 },
    };

    for (first_expected) |sym| {
        const result = first_local.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable in first_local\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try expectSymbolEqual(sym, result);
    }

    const second_expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
        .{ .name = "e", .scope = .local, .index = 0 },
        .{ .name = "f", .scope = .local, .index = 1 },
    };

    for (second_expected) |sym| {
        const result = second_local.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable in second_local\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try expectSymbolEqual(sym, result);
    }
}

test "resolve free" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();
    _ = try global.define("a");
    _ = try global.define("b");

    var first_local = SymbolTable.initEnclosed(testing.allocator, &global);
    defer first_local.deinit();
    _ = try first_local.define("c");
    _ = try first_local.define("d");

    var second_local = SymbolTable.initEnclosed(testing.allocator, &first_local);
    defer second_local.deinit();
    _ = try second_local.define("e");
    _ = try second_local.define("f");

    // first_local: globals resolve as global, locals resolve as local, no free symbols
    const first_expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
        .{ .name = "c", .scope = .local, .index = 0 },
        .{ .name = "d", .scope = .local, .index = 1 },
    };

    for (first_expected) |sym| {
        const result = first_local.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable in first_local\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try expectSymbolEqual(sym, result);
    }

    try testing.expectEqual(@as(usize, 0), first_local.free_symbols.items.len);

    // second_local: globals resolve as global, first_local's locals resolve as free, own locals resolve as local
    const second_expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
        .{ .name = "c", .scope = .free, .index = 0 },
        .{ .name = "d", .scope = .free, .index = 1 },
        .{ .name = "e", .scope = .local, .index = 0 },
        .{ .name = "f", .scope = .local, .index = 1 },
    };

    for (second_expected) |sym| {
        const result = second_local.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable in second_local\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try expectSymbolEqual(sym, result);
    }

    const expected_free = [_]Symbol{
        .{ .name = "c", .scope = .local, .index = 0 },
        .{ .name = "d", .scope = .local, .index = 1 },
    };

    try testing.expectEqual(expected_free.len, second_local.free_symbols.items.len);

    for (expected_free, 0..) |sym, i| {
        const result = second_local.free_symbols.items[i];
        try expectSymbolEqual(sym, result);
    }
}

test "resolve unresolvable free" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();
    _ = try global.define("a");

    var first_local = SymbolTable.initEnclosed(testing.allocator, &global);
    defer first_local.deinit();
    _ = try first_local.define("c");

    var second_local = SymbolTable.initEnclosed(testing.allocator, &first_local);
    defer second_local.deinit();
    _ = try second_local.define("e");
    _ = try second_local.define("f");

    const expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "c", .scope = .free, .index = 0 },
        .{ .name = "e", .scope = .local, .index = 0 },
        .{ .name = "f", .scope = .local, .index = 1 },
    };

    for (expected) |sym| {
        const result = second_local.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try expectSymbolEqual(sym, result);
    }

    const expected_unresolvable = [_][]const u8{ "b", "d" };

    for (expected_unresolvable) |name| {
        const result = second_local.resolve(name);
        if (result != null) {
            std.debug.print("name {s} resolved, but was expected not to\n", .{name});
            return error.TestUnexpectedResult;
        }
    }
}

test "resolve unknown" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();

    const result = global.resolve("x");
    try testing.expect(result == null);
}

test "define and resolve function name" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();
    _ = try global.defineFunctionName("a");

    const expected = Symbol{ .name = "a", .scope = .function, .index = 0 };
    const result = global.resolve("a") orelse {
        std.debug.print("function name 'a' not resolvable\n", .{});
        return error.TestUnexpectedResult;
    };
    try expectSymbolEqual(expected, result);
}

test "shadowing function name" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();
    _ = try global.defineFunctionName("a");
    _ = try global.define("a");

    const expected = Symbol{ .name = "a", .scope = .global, .index = 0 };
    const result = global.resolve("a") orelse {
        std.debug.print("function name 'a' not resolvable\n", .{});
        return error.TestUnexpectedResult;
    };
    try expectSymbolEqual(expected, result);
}
