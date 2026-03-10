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
    store: std.StringHashMap(Symbol),
    num_definitions: usize,

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .outer = null,
            .store = std.StringHashMap(Symbol).init(allocator),
            .num_definitions = 0,
        };
    }

    pub fn define(self: *SymbolTable, name: []const u8) !Symbol {
        const owned_name = try self.store.allocator.dupe(u8, name);
        const symbol = Symbol{
            .name = owned_name,
            .scope = .global,
            .index = self.num_definitions,
        };
        try self.store.put(owned_name, symbol);
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

    pub fn deinit(self: *SymbolTable) void {
        var it = self.store.iterator();
        while (it.next()) |entry| {
            self.store.allocator.free(entry.key_ptr.*);
        }
        self.store.deinit();
    }
};
