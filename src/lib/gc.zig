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
            env.deinit();
            self.allocator.destroy(env);
        }
        self.environments.deinit(self.allocator);
    }

    pub fn allocEnvironment(self: *Self, outer: ?*Environment) !*Environment {
        const env = try self.allocator.create(Environment);
        if (outer) |outer_env| {
            env.* = Environment.initEnclosed(self.allocator, outer_env);
        } else {
            env.* = Environment.init(self.allocator);
        }

        try self.environments.append(self.allocator, env);
        return env;
    }

    pub fn collect(self: *Self, root: *Environment) void {
        self.markEnvironment(root);
        self.sweep();
    }

    pub fn allocFunction(self: *Self, function_obj: Object.Function) !Object {
        const function_ptr = try self.allocator.create(Object.Function);
        errdefer self.allocator.destroy(function_ptr);
        function_ptr.* = function_obj;
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

    fn isTracked(self: *const Self, env: *Environment) bool {
        for (self.environments.items) |tracked_env| {
            if (tracked_env == env) {
                return true;
            }
        }
        return false;
    }

    fn isTrackedError(self: *const Self, err_obj: *Object.Error) bool {
        for (self.errors.items) |tracked_error| {
            if (tracked_error == err_obj) {
                return true;
            }
        }
        return false;
    }

    fn isTrackedFunction(self: *const Self, function_obj: *Object.Function) bool {
        for (self.functions.items) |tracked_function| {
            if (tracked_function == function_obj) {
                return true;
            }
        }
        return false;
    }

    fn isTrackedString(self: *const Self, string_obj: *Object.String) bool {
        for (self.strings.items) |tracked_string| {
            if (tracked_string == string_obj) {
                return true;
            }
        }
        return false;
    }

    fn markObject(self: *Self, obj: Object) void {
        switch (obj) {
            .err => |error_obj| {
                if (!self.isTrackedError(error_obj)) {
                    return;
                }

                if (error_obj.marked) {
                    return;
                }

                error_obj.marked = true;
            },
            .function => |function_obj| {
                if (!self.isTrackedFunction(function_obj)) {
                    return;
                }

                if (function_obj.marked) {
                    return;
                }

                function_obj.marked = true;
                self.markEnvironment(function_obj.environment);
            },
            .string => |string_obj| {
                if (!self.isTrackedString(string_obj)) {
                    return;
                }

                string_obj.marked = true;
            },
            else => {},
        }
    }

    fn markEnvironment(self: *Self, env: *Environment) void {
        if (!self.isTracked(env)) {
            return;
        }

        if (env.marked) {
            return;
        }

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
        var error_index: usize = 0;
        while (error_index < self.errors.items.len) {
            const error_obj = self.errors.items[error_index];
            if (!error_obj.marked) {
                self.allocator.free(error_obj.msg);
                self.allocator.destroy(error_obj);
                _ = self.errors.swapRemove(error_index);
                continue;
            }

            error_obj.marked = false;
            error_index += 1;
        }

        var string_index: usize = 0;
        while (string_index < self.strings.items.len) {
            const string_obj = self.strings.items[string_index];
            if (!string_obj.marked) {
                self.allocator.free(string_obj.value);
                self.allocator.destroy(string_obj);
                _ = self.strings.swapRemove(string_index);
                continue;
            }

            string_obj.marked = false;
            string_index += 1;
        }

        var function_index: usize = 0;
        while (function_index < self.functions.items.len) {
            const function_obj = self.functions.items[function_index];
            if (!function_obj.marked) {
                function_obj.deinit();
                self.allocator.destroy(function_obj);
                _ = self.functions.swapRemove(function_index);
                continue;
            }

            function_obj.marked = false;
            function_index += 1;
        }

        var i: usize = 0;
        while (i < self.environments.items.len) {
            const env = self.environments.items[i];
            if (!env.marked) {
                env.deinit();
                self.allocator.destroy(env);
                _ = self.environments.swapRemove(i);
                continue;
            }

            env.marked = false;
            i += 1;
        }
    }
};
