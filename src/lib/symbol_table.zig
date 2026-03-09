const std = @import("std");

pub const SymbolScope = enum {
    global,
};

pub const Symbol = struct {
    name: []const u8,
    scope: SymbolScope,
    index: usize,
};

pub const SymbolTable = struct {
    outer: ?*SymbolTable,
    store: std.StringHashMapUnmanaged(Symbol),
    num_definitions: usize,

    pub fn init() SymbolTable {
        return .{
            .outer = null,
            .store = .empty,
            .num_definitions = 0,
        };
    }

    pub fn define(self: *SymbolTable, alloc: std.mem.Allocator, name: []const u8) !Symbol {
        const symbol = Symbol{
            .name = name,
            .scope = .global,
            .index = self.num_definitions,
        };
        try self.store.put(alloc, name, symbol);
        self.num_definitions += 1;
        return symbol;
    }

    pub fn resolve(self: *const SymbolTable, name: []const u8) ?Symbol {
        if (self.store.get(name)) |symbol| {
            return symbol;
        }
        if (self.outer) |outer| {
            return outer.resolve(name);
        }
        return null;
    }

    pub fn deinit(self: *SymbolTable, alloc: std.mem.Allocator) void {
        self.store.deinit(alloc);
    }
};
