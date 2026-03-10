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
    const expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
    };

    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();

    const a = try global.define("a");
    try expectSymbolEqual(expected[0], a);

    const b = try global.define("b");
    try expectSymbolEqual(expected[1], b);
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

test "resolve unknown" {
    var global = SymbolTable.init(testing.allocator);
    defer global.deinit();

    const result = global.resolve("x");
    try testing.expect(result == null);
}
