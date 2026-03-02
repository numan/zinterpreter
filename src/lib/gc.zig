const std = @import("std");
const object = @import("object.zig");
const environment = @import("environment.zig");

const Object = object.Object;
const Environment = environment.Environment;

pub const Gc = struct {
    allocator: std.mem.Allocator,
    environments: std.ArrayList(*Environment) = .{},
    functions: std.ArrayList(*Object.Function) = .{},
    strings: std.ArrayList(*Object.String) = .{},
    arrays: std.ArrayList(*Object.Array) = .{},

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .environments = .{},
            .functions = .{},
            .strings = .{},
            .arrays = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.strings.items) |string_obj| {
            self.allocator.free(string_obj.value);
            self.allocator.destroy(string_obj);
        }
        self.strings.deinit(self.allocator);

        for (self.functions.items) |function_obj| {
            function_obj.deinit();
            self.allocator.destroy(function_obj);
        }
        self.functions.deinit(self.allocator);

        for (self.arrays.items) |array_obj| {
            self.allocator.free(array_obj.elements);
            self.allocator.destroy(array_obj);
        }
        self.arrays.deinit(self.allocator);

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
        const function_ptr = try self.allocator.create(Object.Function);
        errdefer self.allocator.destroy(function_ptr);
        function_ptr.* = function_obj;
        self.retainEnvironment(function_ptr.environment);
        try self.functions.append(self.allocator, function_ptr);
        return .{ .function = function_ptr };
    }

    pub fn allocString(self: *Self, value: []const u8) !Object {
        const duped_value = try self.allocator.dupe(u8, value);
        errdefer self.allocator.free(duped_value);
        const string_ptr = try self.allocator.create(Object.String);
        errdefer self.allocator.destroy(string_ptr);
        string_ptr.* = .{ .value = duped_value, .marked = false };
        try self.strings.append(self.allocator, string_ptr);
        return .{ .string = string_ptr };
    }

    pub fn allocStringConcat(self: *Self, left: []const u8, right: []const u8) !Object {
        const buf = try self.allocator.alloc(u8, left.len + right.len);
        errdefer self.allocator.free(buf);
        @memcpy(buf[0..left.len], left);
        @memcpy(buf[left.len..], right);
        const string_ptr = try self.allocator.create(Object.String);
        errdefer self.allocator.destroy(string_ptr);
        string_ptr.* = .{ .value = buf, .marked = false };
        try self.strings.append(self.allocator, string_ptr);
        return .{ .string = string_ptr };
    }

    pub fn allocArray(self: *Self, elements: []const Object) !Object {
        const duped_elements = try self.allocator.dupe(Object, elements);
        errdefer self.allocator.free(duped_elements);
        for (duped_elements) |elem| {
            self.retainObject(elem);
        }
        const array_ptr = try self.allocator.create(Object.Array);
        errdefer self.allocator.destroy(array_ptr);
        array_ptr.* = .{ .elements = duped_elements, .marked = false };
        try self.arrays.append(self.allocator, array_ptr);
        return .{ .array = array_ptr };
    }

    // --- Reference counting: retain/decrement ---

    pub fn retainObject(self: *Self, obj: Object) void {
        _ = self;
        switch (obj) {
            .function => |f| f.ref_count += 1,
            .string => |s| s.ref_count += 1,
            .array => |a| a.ref_count += 1,
            else => {},
        }
    }

    pub fn decrementRefCount(self: *Self, obj: Object) void {
        _ = self;
        switch (obj) {
            .function => |f| f.ref_count -= 1,
            .string => |s| s.ref_count -= 1,
            .array => |a| a.ref_count -= 1,
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

    // --- Mark and sweep ---

    pub fn collect(self: *Self, root: *Environment) void {
        self.markEnvironment(root);
        self.sweep();
    }

    fn markObject(self: *Self, obj: Object) void {
        switch (obj) {
            .function => |function_obj| {
                if (!isInList(*Object.Function, self.functions.items, function_obj)) return;
                if (function_obj.marked) return;
                function_obj.marked = true;
                self.markEnvironment(function_obj.environment);
            },
            .string => |string_obj| {
                if (!isInList(*Object.String, self.strings.items, string_obj)) return;
                string_obj.marked = true;
            },
            .array => |array_obj| {
                if (!isInList(*Object.Array, self.arrays.items, array_obj)) return;
                if (array_obj.marked) return;
                array_obj.marked = true;
                for (array_obj.elements) |elem| {
                    self.markObject(elem);
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
        self.sweepList(*Object.String, &self.strings, sweepFreeString);
        self.sweepList(*Object.Function, &self.functions, sweepFreeFunction);
        self.sweepList(*Object.Array, &self.arrays, sweepFreeArray);
        self.sweepList(*Environment, &self.environments, sweepFreeEnvironment);
    }

    /// For unmarked (dying) objects, decrement ref_counts of their marked (surviving)
    /// outgoing references. This keeps surviving objects' ref_counts accurate after
    /// the dying objects are freed without RC cascading.
    fn adjustSurvivingRefCounts(self: *Self) void {
        for (self.functions.items) |function_obj| {
            if (!function_obj.marked) {
                const captured_env = function_obj.environment;
                if (captured_env.marked) {
                    captured_env.ref_count -|= 1;
                }
            }
        }

        for (self.arrays.items) |array_obj| {
            if (!array_obj.marked) {
                for (array_obj.elements) |elem| {
                    switch (elem) {
                        .string => |s| if (s.marked) {
                            s.ref_count -|= 1;
                        },
                        .function => |f| if (f.marked) {
                            f.ref_count -|= 1;
                        },
                        .array => |a| if (a.marked) {
                            a.ref_count -|= 1;
                        },
                        else => {},
                    }
                }
            }
        }

        for (self.environments.items) |env| {
            if (!env.marked) {
                var iterator = env.storage.iterator();
                while (iterator.next()) |entry| {
                    switch (entry.value_ptr.*) {
                        .function => |f| if (f.marked) {
                            f.ref_count -|= 1;
                        },
                        .string => |s| if (s.marked) {
                            s.ref_count -|= 1;
                        },
                        .array => |a| if (a.marked) {
                            a.ref_count -|= 1;
                        },
                        else => {},
                    }
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

    fn sweepFreeString(self: *Self, string_obj: *Object.String) void {
        self.allocator.free(string_obj.value);
        self.allocator.destroy(string_obj);
    }

    fn sweepFreeFunction(self: *Self, function_obj: *Object.Function) void {
        function_obj.deinit();
        self.allocator.destroy(function_obj);
    }

    fn sweepFreeArray(self: *Self, array_obj: *Object.Array) void {
        self.allocator.free(array_obj.elements);
        self.allocator.destroy(array_obj);
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

    fn isInList(comptime T: type, list: []const T, needle: T) bool {
        for (list) |item| {
            if (item == needle) return true;
        }
        return false;
    }
};
