const std = @import("std");
const object = @import("object.zig");
const Object = object.Object;

const StorageHashMap = std.StringHashMap(Object);

pub const Environment = struct {
    allocator: std.mem.Allocator,
    storage: StorageHashMap,
    outer: ?*const Environment,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Environment {
        return Environment{
            .allocator = allocator,
            .storage = StorageHashMap.init(allocator),
            .outer = null,
        };
    }

    pub fn initEnclosed(allocator: std.mem.Allocator, outer: *const Environment) Environment {
        var env = Environment.init(allocator);
        env.outer = outer;
        return env;
    }

    pub fn deinit(self: *Self) void {
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
        try self.storage.put(key, value);
        return value;
    }
};
