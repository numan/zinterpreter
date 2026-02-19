const std = @import("std");
const object = @import("object.zig");
const Object = object.Object;

const StorageHashMap = std.StringHashMap(Object);

pub const Environment = struct {
    allocator: std.mem.Allocator,
    storage: StorageHashMap,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Environment {
        return Environment{
            .allocator = allocator,
            .storage = StorageHashMap.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.storage.deinit();
    }

    pub fn get(self: *const Self, key: []const u8) ?Object {
        return self.storage.get(key);
    }

    pub fn set(self: *Self, key: []const u8, value: Object) !Object {
        try self.storage.put(key, value);
        return value;
    }
};
