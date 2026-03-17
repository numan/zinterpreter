const std = @import("std");
const builtins = @import("builtins.zig");

pub const SymbolScope = enum {
    global,
    local,
    builtin,
    free,
};

pub const Symbol = struct {
    name: []const u8,
    scope: SymbolScope,
    index: usize,
};

pub const SymbolTable = struct {
    outer: ?*SymbolTable,
    allocator: std.mem.Allocator,
    store: std.StringHashMap(Symbol),
    num_definitions: usize,
    free_symbols: std.ArrayList(Symbol),

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .outer = null,
            .allocator = allocator,
            .store = std.StringHashMap(Symbol).init(allocator),
            .num_definitions = 0,
            .free_symbols = .empty,
        };
    }

    pub fn initEnclosed(allocator: std.mem.Allocator, outer: *SymbolTable) SymbolTable {
        return .{
            .outer = outer,
            .allocator = allocator,
            .store = std.StringHashMap(Symbol).init(allocator),
            .num_definitions = 0,
            .free_symbols = .empty,
        };
    }

    pub fn define(self: *SymbolTable, name: []const u8) !Symbol {
        const owned_name = try self.store.allocator.dupe(u8, name);
        const symbol = Symbol{
            .name = owned_name,
            .scope = if (self.outer == null) .global else .local,
            .index = self.num_definitions,
        };
        try self.store.put(owned_name, symbol);
        self.num_definitions += 1;
        return symbol;
    }

    pub fn resolve(self: *SymbolTable, name: []const u8) ?Symbol {
        if (self.store.get(name)) |symbol| {
            return symbol;
        }
        if (self.outer) |outer| {
            const obj = outer.resolve(name) orelse return null;
            if (obj.scope == .global or obj.scope == .builtin) {
                return obj;
            }
            return self.defineFree(obj) catch return null;
        }
        if (builtins.fromName(name)) |b| {
            return Symbol{
                .name = name,
                .scope = .builtin,
                .index = @intFromEnum(b),
            };
        }
        return null;
    }

    fn defineFree(self: *SymbolTable, original: Symbol) !Symbol {
        try self.free_symbols.append(self.store.allocator, original);

        const owned_name = try self.store.allocator.dupe(u8, original.name);
        const free_copy: Symbol = .{
            .name = owned_name,
            .index = self.free_symbols.items.len - 1,
            .scope = .free,
        };

        try self.store.put(owned_name, free_copy);
        return free_copy;
    }

    pub fn deinit(self: *SymbolTable) void {
        var it = self.store.iterator();
        while (it.next()) |entry| {
            self.store.allocator.free(entry.key_ptr.*);
        }
        self.store.deinit();
        self.free_symbols.deinit(self.allocator);
    }
};
