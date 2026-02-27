const std = @import("std");
const object = @import("object.zig");
const environment = @import("environment.zig");

const Object = object.Object;
const Environment = environment.Environment;

pub const Gc = struct {
    allocator: std.mem.Allocator,
    environments: std.ArrayList(*Environment) = .{},
    errors: std.ArrayList(*Object.Error) = .{},
    functions: std.ArrayList(*Object.Function) = .{},
    strings: std.ArrayList(*Object.String) = .{},

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .environments = .{},
            .errors = .{},
            .functions = .{},
            .strings = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.errors.items) |err_obj| {
            self.allocator.free(err_obj.msg);
            self.allocator.destroy(err_obj);
        }
        self.errors.deinit(self.allocator);

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

    pub fn allocErrorOwned(self: *Self, msg: []const u8) !Object {
        const error_ptr = try self.allocator.create(Object.Error);
        error_ptr.* = Object.Error.init(msg);
        try self.errors.append(self.allocator, error_ptr);
        return .{ .err = error_ptr };
    }

    // --- Reference counting: retain/release ---

    pub fn retainObject(self: *Self, obj: Object) void {
        _ = self;
        switch (obj) {
            .err => |e| e.ref_count += 1,
            .function => |f| f.ref_count += 1,
            .string => |s| s.ref_count += 1,
            else => {},
        }
    }

    pub fn releaseObject(self: *Self, obj: Object) void {
        switch (obj) {
            .err => |e| {
                e.ref_count -= 1;
                if (e.ref_count == 0) self.freeError(e);
            },
            .function => |f| {
                f.ref_count -= 1;
                if (f.ref_count == 0) self.freeFunction(f);
            },
            .string => |s| {
                s.ref_count -= 1;
                if (s.ref_count == 0) self.freeString(s);
            },
            else => {},
        }
    }

    pub fn retainEnvironment(self: *Self, env: *Environment) void {
        _ = self;
        env.ref_count += 1;
    }

    pub fn releaseEnvironment(self: *Self, env: *Environment) void {
        env.ref_count -= 1;
        if (env.ref_count == 0) self.freeEnvironment(env);
    }

    // --- Reference counting: free helpers ---

    fn freeError(self: *Self, err_obj: *Object.Error) void {
        self.removeFromList(*Object.Error, &self.errors, err_obj);
        self.allocator.free(err_obj.msg);
        self.allocator.destroy(err_obj);
    }

    fn freeString(self: *Self, string_obj: *Object.String) void {
        self.removeFromList(*Object.String, &self.strings, string_obj);
        self.allocator.free(string_obj.value);
        self.allocator.destroy(string_obj);
    }

    fn freeFunction(self: *Self, function_obj: *Object.Function) void {
        self.removeFromList(*Object.Function, &self.functions, function_obj);
        // Cascade: release captured environment
        self.releaseEnvironment(function_obj.environment);
        function_obj.deinit();
        self.allocator.destroy(function_obj);
    }

    fn freeEnvironment(self: *Self, env: *Environment) void {
        self.removeFromList(*Environment, &self.environments, env);
        // Cascade: release all stored objects and outer env
        var iterator = env.storage.iterator();
        while (iterator.next()) |entry| {
            self.releaseObject(entry.value_ptr.*);
            self.allocator.free(entry.key_ptr.*);
        }
        env.storage.deinit();
        if (env.outer) |outer| {
            self.releaseEnvironment(outer);
        }
        self.allocator.destroy(env);
    }

    fn removeFromList(self: *Self, comptime T: type, list: *std.ArrayList(T), item: T) void {
        _ = self;
        for (list.items, 0..) |tracked, i| {
            if (tracked == item) {
                _ = list.swapRemove(i);
                return;
            }
        }
    }

    // --- GC-aware environment set ---

    fn replaceValue(self: *Self, existing_value: *Object, value: Object) Object {
        // Retain new before releasing old (handles same-object case)
        self.retainObject(value);
        self.releaseObject(existing_value.*);
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
            .err => |error_obj| {
                if (!isInList(*Object.Error, self.errors.items, error_obj)) return;
                if (error_obj.marked) return;
                error_obj.marked = true;
            },
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
        self.sweepList(*Object.Error, &self.errors, sweepFreeError);
        self.sweepList(*Object.String, &self.strings, sweepFreeString);
        self.sweepList(*Object.Function, &self.functions, sweepFreeFunction);
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

        for (self.environments.items) |env| {
            if (!env.marked) {
                var iterator = env.storage.iterator();
                while (iterator.next()) |entry| {
                    switch (entry.value_ptr.*) {
                        .err => |e| if (e.marked) {
                            e.ref_count -|= 1;
                        },
                        .function => |f| if (f.marked) {
                            f.ref_count -|= 1;
                        },
                        .string => |s| if (s.marked) {
                            s.ref_count -|= 1;
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

    fn sweepFreeError(self: *Self, err_obj: *Object.Error) void {
        self.allocator.free(err_obj.msg);
        self.allocator.destroy(err_obj);
    }

    fn sweepFreeString(self: *Self, string_obj: *Object.String) void {
        self.allocator.free(string_obj.value);
        self.allocator.destroy(string_obj);
    }

    fn sweepFreeFunction(self: *Self, function_obj: *Object.Function) void {
        function_obj.deinit();
        self.allocator.destroy(function_obj);
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

    pub fn trackedErrorCount(self: *const Self) usize {
        return self.errors.items.len;
    }

    pub fn trackedFunctionCount(self: *const Self) usize {
        return self.functions.items.len;
    }

    pub fn trackedStringCount(self: *const Self) usize {
        return self.strings.items.len;
    }

    fn isInList(comptime T: type, list: []const T, needle: T) bool {
        for (list) |item| {
            if (item == needle) return true;
        }
        return false;
    }
};
