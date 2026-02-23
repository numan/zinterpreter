const std = @import("std");
const object = @import("object.zig");
const Object = object.Object;

const StorageHashMap = std.StringHashMap(Object);

pub const Environment = struct {
    allocator: std.mem.Allocator,
    storage: StorageHashMap,
    outer: ?*Environment,
    marked: bool = false,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Environment {
        return Environment{
            .allocator = allocator,
            .storage = StorageHashMap.init(allocator),
            .outer = null,
            .marked = false,
        };
    }

    pub fn initEnclosed(allocator: std.mem.Allocator, outer: *Environment) Environment {
        var env = Environment.init(allocator);
        env.outer = outer;
        return env;
    }

    pub fn deinit(self: *Self) void {
        var iterator = self.storage.iterator();
        while (iterator.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.storage.deinit();
    }

    pub fn get(self: *const Self, key: []const u8) ?Object {
        if (self.storage.get(key)) |value| {
            return value;
        }

        if (self.outer) |outer| {
            return outer.get(key);
        }

        return null;
    }

    pub fn set(self: *Self, key: []const u8, value: Object) !Object {
        if (self.storage.getPtr(key)) |existing_value| {
            existing_value.* = value;
            return value;
        }

        const owned_key = try self.allocator.dupe(u8, key);
        try self.storage.put(owned_key, value);
        return value;
    }
};
