const std = @import("std");
const object = @import("object.zig");
const environment = @import("environment.zig");

const Object = object.Object;
const Environment = environment.Environment;

/// Wraps a heap-allocated object type with GC metadata.
/// The Object union stores *T pointing to the `.data` field;
/// use `gcMeta(T, ptr)` to recover the wrapper via @fieldParentPtr.
/// Only objects allocated through Gc carry this wrapper — VM-allocated
/// objects must never be passed to retainObject/decrementRefCount.
fn GcManaged(comptime T: type) type {
    return struct {
        data: T,
        marked: bool = false,
        ref_count: usize = 0,
    };
}

fn gcMeta(comptime T: type, ptr: *T) *GcManaged(T) {
    return @fieldParentPtr("data", ptr);
}

pub const Gc = struct {
    allocator: std.mem.Allocator,
    environments: std.ArrayList(*Environment),
    functions: std.ArrayList(*GcManaged(Object.Function)),
    strings: std.ArrayList(*GcManaged(Object.String)),
    arrays: std.ArrayList(*GcManaged(Object.Array)),
    hashes: std.ArrayList(*GcManaged(Object.Hash)),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .environments = .empty,
            .functions = .empty,
            .strings = .empty,
            .arrays = .empty,
            .hashes = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.strings.items) |managed| {
            self.allocator.free(managed.data.value);
            self.allocator.destroy(managed);
        }
        self.strings.deinit(self.allocator);

        for (self.functions.items) |managed| {
            managed.data.deinit();
            self.allocator.destroy(managed);
        }
        self.functions.deinit(self.allocator);

        for (self.arrays.items) |managed| {
            self.allocator.free(managed.data.elements);
            self.allocator.destroy(managed);
        }
        self.arrays.deinit(self.allocator);

        for (self.hashes.items) |managed| {
            managed.data.pairs.deinit();
            self.allocator.destroy(managed);
        }
        self.hashes.deinit(self.allocator);

        for (self.environments.items) |env| {
            var iterator = env.storage.iterator();
            while (iterator.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
            env.storage.deinit();
            self.allocator.destroy(env);
        }
        self.environments.deinit(self.allocator);
    }

    // --- Allocation ---

    pub fn allocEnvironment(self: *Self, outer: ?*Environment) !*Environment {
        const env = try self.allocator.create(Environment);
        if (outer) |outer_env| {
            env.* = Environment.initEnclosed(self.allocator, outer_env);
            self.retainEnvironment(outer_env);
        } else {
            env.* = Environment.init(self.allocator);
        }

        try self.environments.append(self.allocator, env);
        return env;
    }

    pub fn allocFunction(self: *Self, function_obj: Object.Function) !Object {
        const managed = try self.allocator.create(GcManaged(Object.Function));
        errdefer self.allocator.destroy(managed);
        managed.* = .{ .data = function_obj };
        self.retainEnvironment(managed.data.environment);
        try self.functions.append(self.allocator, managed);
        return .{ .function = &managed.data };
    }

    pub fn allocString(self: *Self, value: []const u8) !Object {
        const duped_value = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(duped_value);
        const managed = try self.allocator.create(GcManaged(Object.String));
        errdefer self.allocator.destroy(managed);
        managed.* = .{ .data = .{ .value = duped_value } };
        try self.strings.append(self.allocator, managed);
        return .{ .string = &managed.data };
    }

    pub fn allocStringConcat(self: *Self, left: []const u8, right: []const u8) !Object {
        const buf = try self.allocator.alloc(u8, left.len + right.len);
        errdefer self.allocator.free(buf);
        @memcpy(buf[0..left.len], left);
        @memcpy(buf[left.len..], right);
        const managed = try self.allocator.create(GcManaged(Object.String));
        errdefer self.allocator.destroy(managed);
        managed.* = .{ .data = .{ .value = buf } };
        try self.strings.append(self.allocator, managed);
        return .{ .string = &managed.data };
    }

    pub fn allocArray(self: *Self, elements: []const Object) !Object {
        const duped_elements = try self.allocator.dupe(Object, elements);
        errdefer self.allocator.free(duped_elements);
        for (duped_elements) |elem| {
            self.retainObject(elem);
        }
        const managed = try self.allocator.create(GcManaged(Object.Array));
        errdefer self.allocator.destroy(managed);
        managed.* = .{ .data = .{ .elements = duped_elements } };
        try self.arrays.append(self.allocator, managed);
        return .{ .array = &managed.data };
    }

    pub fn allocHash(self: *Self, pairs: std.AutoHashMap(Object.HashKey, Object.HashPair)) !Object {
        const managed = try self.allocator.create(GcManaged(Object.Hash));
        errdefer self.allocator.destroy(managed);
        managed.* = .{ .data = .{ .pairs = pairs } };
        var iterator = managed.data.pairs.iterator();
        while (iterator.next()) |entry| {
            self.retainObject(entry.value_ptr.key);
            self.retainObject(entry.value_ptr.value);
        }
        try self.hashes.append(self.allocator, managed);
        return .{ .hash = &managed.data };
    }

    /// Wraps an externally-created array into GcManaged tracking.
    /// Frees the original *Array and returns the new *Array inside the wrapper.
    pub fn trackArray(self: *Self, arr: *Object.Array) !*Object.Array {
        const managed = try self.allocator.create(GcManaged(Object.Array));
        managed.* = .{ .data = arr.* };
        self.allocator.destroy(arr);
        try self.arrays.append(self.allocator, managed);
        return &managed.data;
    }

    // --- Reference counting: retain/decrement ---
    // NOTE: These methods assume the object was allocated through Gc
    // (i.e. wrapped in GcManaged). Calling with VM-allocated objects is UB.

    pub fn retainObject(self: *Self, obj: Object) void {
        _ = self;
        switch (obj) {
            .function => |f| gcMeta(Object.Function, f).ref_count += 1,
            .string => |s| gcMeta(Object.String, s).ref_count += 1,
            .array => |a| gcMeta(Object.Array, a).ref_count += 1,
            .hash => |h| gcMeta(Object.Hash, h).ref_count += 1,
            else => {},
        }
    }

    pub fn decrementRefCount(self: *Self, obj: Object) void {
        _ = self;
        switch (obj) {
            .function => |f| gcMeta(Object.Function, f).ref_count -= 1,
            .string => |s| gcMeta(Object.String, s).ref_count -= 1,
            .array => |a| gcMeta(Object.Array, a).ref_count -= 1,
            .hash => |h| gcMeta(Object.Hash, h).ref_count -= 1,
            else => {},
        }
    }

    pub fn retainEnvironment(self: *Self, env: *Environment) void {
        _ = self;
        env.ref_count += 1;
    }

    pub fn decrementEnvRefCount(self: *Self, env: *Environment) void {
        _ = self;
        env.ref_count -= 1;
    }

    // --- GC-aware environment set ---

    fn replaceValue(self: *Self, existing_value: *Object, value: Object) Object {
        // Retain new before releasing old (handles same-object case)
        self.retainObject(value);
        self.decrementRefCount(existing_value.*);
        existing_value.* = value;
        return value;
    }

    pub fn envSet(self: *Self, env: *Environment, key: []const u8, value: Object) !Object {
        if (env.storage.getPtr(key)) |existing_value| {
            return self.replaceValue(existing_value, value);
        }

        self.retainObject(value);
        const owned_key = try self.allocator.dupe(u8, key);
        try env.storage.put(owned_key, value);
        return value;
    }

    pub fn envSetExisting(self: *Self, env: *Environment, key: []const u8, value: Object) ?Object {
        if (env.storage.getPtr(key)) |existing_value| {
            return self.replaceValue(existing_value, value);
        }

        if (env.outer) |outer| {
            return self.envSetExisting(outer, key, value);
        }

        return null;
    }

    // --- Public helpers ---

    /// Returns the GC ref_count for a managed object. For testing.
    pub fn refCount(self: *const Self, obj: Object) usize {
        _ = self;
        return switch (obj) {
            .function => |f| gcMeta(Object.Function, f).ref_count,
            .string => |s| gcMeta(Object.String, s).ref_count,
            .array => |a| gcMeta(Object.Array, a).ref_count,
            .hash => |h| gcMeta(Object.Hash, h).ref_count,
            else => 0,
        };
    }

    // --- Mark and sweep ---

    pub fn collect(self: *Self, root: *Environment) void {
        self.markEnvironment(root);
        self.sweep();
    }

    fn markObject(self: *Self, obj: Object) void {
        switch (obj) {
            .function => |function_obj| {
                const managed = gcMeta(Object.Function, function_obj);
                if (!isInList(*GcManaged(Object.Function), self.functions.items, managed)) return;
                if (managed.marked) return;
                managed.marked = true;
                self.markEnvironment(function_obj.environment);
            },
            .string => |string_obj| {
                const managed = gcMeta(Object.String, string_obj);
                if (!isInList(*GcManaged(Object.String), self.strings.items, managed)) return;
                managed.marked = true;
            },
            .array => |array_obj| {
                const managed = gcMeta(Object.Array, array_obj);
                if (!isInList(*GcManaged(Object.Array), self.arrays.items, managed)) return;
                if (managed.marked) return;
                managed.marked = true;
                for (array_obj.elements) |elem| {
                    self.markObject(elem);
                }
            },
            .hash => |hash_obj| {
                const managed = gcMeta(Object.Hash, hash_obj);
                if (!isInList(*GcManaged(Object.Hash), self.hashes.items, managed)) return;
                if (managed.marked) return;
                managed.marked = true;
                var iterator = hash_obj.pairs.iterator();
                while (iterator.next()) |entry| {
                    self.markObject(entry.value_ptr.key);
                    self.markObject(entry.value_ptr.value);
                }
            },
            else => {},
        }
    }

    fn markEnvironment(self: *Self, env: *Environment) void {
        if (!isInList(*Environment, self.environments.items, env)) return;
        if (env.marked) return;

        env.marked = true;

        if (env.outer) |outer| {
            self.markEnvironment(outer);
        }

        var iterator = env.storage.iterator();
        while (iterator.next()) |entry| {
            self.markObject(entry.value_ptr.*);
        }
    }

    fn sweep(self: *Self) void {
        self.adjustSurvivingRefCounts();
        self.sweepList(*GcManaged(Object.String), &self.strings, sweepFreeString);
        self.sweepList(*GcManaged(Object.Function), &self.functions, sweepFreeFunction);
        self.sweepList(*GcManaged(Object.Array), &self.arrays, sweepFreeArray);
        self.sweepList(*GcManaged(Object.Hash), &self.hashes, sweepFreeHash);
        self.sweepList(*Environment, &self.environments, sweepFreeEnvironment);
    }

    /// For unmarked (dying) objects, decrement ref_counts of their marked (surviving)
    /// outgoing references. This keeps surviving objects' ref_counts accurate after
    /// the dying objects are freed without RC cascading.
    fn adjustSurvivingRefCounts(self: *Self) void {
        for (self.functions.items) |managed| {
            if (!managed.marked) {
                const captured_env = managed.data.environment;
                if (captured_env.marked) {
                    captured_env.ref_count -|= 1;
                }
            }
        }

        for (self.arrays.items) |managed| {
            if (!managed.marked) {
                for (managed.data.elements) |elem| {
                    self.adjustRefForDying(elem);
                }
            }
        }

        for (self.hashes.items) |managed| {
            if (!managed.marked) {
                var iterator = managed.data.pairs.iterator();
                while (iterator.next()) |entry| {
                    self.adjustRefForDying(entry.value_ptr.key);
                    self.adjustRefForDying(entry.value_ptr.value);
                }
            }
        }

        for (self.environments.items) |env| {
            if (!env.marked) {
                var iterator = env.storage.iterator();
                while (iterator.next()) |entry| {
                    self.adjustRefForDying(entry.value_ptr.*);
                }
                if (env.outer) |outer| {
                    if (outer.marked) {
                        outer.ref_count -|= 1;
                    }
                }
            }
        }
    }

    fn sweepList(self: *Self, comptime T: type, list: *std.ArrayList(T), comptime freeFn: fn (*Self, T) void) void {
        var i: usize = 0;
        while (i < list.items.len) {
            const item = list.items[i];
            if (!item.marked) {
                freeFn(self, item);
                _ = list.swapRemove(i);
            } else {
                item.marked = false;
                i += 1;
            }
        }
    }

    fn sweepFreeString(self: *Self, managed: *GcManaged(Object.String)) void {
        self.allocator.free(managed.data.value);
        self.allocator.destroy(managed);
    }

    fn sweepFreeFunction(self: *Self, managed: *GcManaged(Object.Function)) void {
        managed.data.deinit();
        self.allocator.destroy(managed);
    }

    fn sweepFreeArray(self: *Self, managed: *GcManaged(Object.Array)) void {
        self.allocator.free(managed.data.elements);
        self.allocator.destroy(managed);
    }

    fn sweepFreeHash(self: *Self, managed: *GcManaged(Object.Hash)) void {
        managed.data.pairs.deinit();
        self.allocator.destroy(managed);
    }

    fn sweepFreeEnvironment(self: *Self, env: *Environment) void {
        var iterator = env.storage.iterator();
        while (iterator.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        env.storage.deinit();
        self.allocator.destroy(env);
    }

    pub fn trackedEnvironmentCount(self: *const Self) usize {
        return self.environments.items.len;
    }

    pub fn trackedFunctionCount(self: *const Self) usize {
        return self.functions.items.len;
    }

    pub fn trackedStringCount(self: *const Self) usize {
        return self.strings.items.len;
    }

    pub fn trackedArrayCount(self: *const Self) usize {
        return self.arrays.items.len;
    }

    pub fn trackedHashCount(self: *const Self) usize {
        return self.hashes.items.len;
    }

    fn adjustRefForDying(self: *Self, obj: Object) void {
        _ = self;
        switch (obj) {
            .string => |s| {
                const m = gcMeta(Object.String, s);
                if (m.marked) m.ref_count -|= 1;
            },
            .function => |f| {
                const m = gcMeta(Object.Function, f);
                if (m.marked) m.ref_count -|= 1;
            },
            .array => |a| {
                const m = gcMeta(Object.Array, a);
                if (m.marked) m.ref_count -|= 1;
            },
            .hash => |h| {
                const m = gcMeta(Object.Hash, h);
                if (m.marked) m.ref_count -|= 1;
            },
            else => {},
        }
    }

    fn isInList(comptime T: type, list: []const T, needle: T) bool {
        for (list) |item| {
            if (item == needle) return true;
        }
        return false;
    }
};
