const std = @import("std");
const testing = std.testing;
const SymbolTable = @import("symbol_table.zig").SymbolTable;
const Symbol = @import("symbol_table.zig").Symbol;
const SymbolScope = @import("symbol_table.zig").SymbolScope;

test "define" {
    const expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
    };

    var global = SymbolTable.init();
    defer global.deinit(testing.allocator);

    const a = try global.define(testing.allocator, "a");
    try testing.expectEqual(expected[0], a);

    const b = try global.define(testing.allocator, "b");
    try testing.expectEqual(expected[1], b);
}

test "resolve global" {
    var global = SymbolTable.init();
    defer global.deinit(testing.allocator);

    _ = try global.define(testing.allocator, "a");
    _ = try global.define(testing.allocator, "b");

    const expected = [_]Symbol{
        .{ .name = "a", .scope = .global, .index = 0 },
        .{ .name = "b", .scope = .global, .index = 1 },
    };

    for (expected) |sym| {
        const result = global.resolve(sym.name) orelse {
            std.debug.print("name {s} not resolvable\n", .{sym.name});
            return error.TestUnexpectedResult;
        };
        try testing.expectEqual(sym, result);
    }
}

test "resolve unknown" {
    var global = SymbolTable.init();
    defer global.deinit(testing.allocator);

    const result = global.resolve("x");
    try testing.expect(result == null);
}
