const std = @import("std");
const ast = @import("ast.zig");
const environment = @import("environment.zig");
const evaluator_mod = @import("evaluator.zig");

pub const BuiltinFnType = *const fn (*evaluator_mod.Evaluator, []const Object) std.mem.Allocator.Error!Object;

pub const Object = union(enum) {
    int: Integer,
    bool: Boolean,
    null: Null,
    err: Error,
    function: *Function,
    string: *String,
    array: *Array,
    hash: *Hash,
    builtin: Builtin,

    const Self = @This();

    pub const HashKey = struct {
        obj_type: enum { int, bool, string },
        value: u64,
    };

    pub const Builtin = struct {
        function: BuiltinFnType,

        pub fn init(func: BuiltinFnType) Builtin {
            return .{ .function = func };
        }

        pub fn inspect(self: *const Builtin, writer: *std.Io.Writer) !void {
            _ = self;
            try writer.writeAll("builtin function");
            try writer.flush();
        }
    };

    pub const Error = struct {
        msg: []const u8,

        pub fn init(m: []const u8) Error {
            return .{ .msg = m };
        }

        pub fn inspect(self: *const Object.Error, writer: *std.Io.Writer) !void {
            try writer.writeAll("ERROR: ");
            try writer.writeAll(self.msg);
            try writer.flush();
        }
    };

    pub const String = struct {
        value: []const u8,
        marked: bool = false,
        ref_count: usize = 0,

        pub fn init(value: []const u8) String {
            return String{
                .value = value,
                .marked = false,
            };
        }

        pub fn hashKey(self: *const String) HashKey {
            return .{ .obj_type = .string, .value = std.hash.Fnv1a_64.hash(self.value) };
        }

        pub fn inspect(self: *const Object.String, writer: *std.Io.Writer) !void {
            try writer.writeAll(self.value);
            try writer.flush();
        }
    };

    pub const Integer = struct {
        value: i64,

        pub fn init(value: i64) Integer {
            return Integer{
                .value = value,
            };
        }

        pub fn hashKey(self: *const Integer) HashKey {
            return .{ .obj_type = .int, .value = @bitCast(self.value) };
        }

        pub fn inspect(self: *const Object.Integer, writer: *std.Io.Writer) !void {
            try writer.print("{d}", .{self.value});
            try writer.flush();
        }
    };

    pub const Boolean = struct {
        value: bool,

        pub fn init(value: bool) Boolean {
            return Boolean{
                .value = value,
            };
        }

        pub fn hashKey(self: *const Boolean) HashKey {
            return .{ .obj_type = .bool, .value = if (self.value) 1 else 0 };
        }

        pub fn inspect(self: *const Object.Boolean, writer: *std.Io.Writer) !void {
            try writer.print("{}", .{self.value});
            try writer.flush();
        }
    };

    pub const Null = struct {
        const empty = .{};

        pub fn init() Null {
            return Null{};
        }
        pub fn inspect(self: *const Object.Null, writer: *std.Io.Writer) !void {
            _ = self;
            try writer.writeAll("null");
            try writer.flush();
        }
    };

    pub const Function = struct {
        parameters: []const ast.Identifier,
        body: *const ast.StatementType.BlockStatement,
        environment: *environment.Environment,
        marked: bool = false,
        ref_count: usize = 0,
        arena: std.heap.ArenaAllocator,

        pub fn init(
            parameters: []const ast.Identifier,
            body: *const ast.StatementType.BlockStatement,
            env: *environment.Environment,
            arena: std.heap.ArenaAllocator,
        ) Function {
            return .{
                .parameters = parameters,
                .body = body,
                .environment = env,
                .marked = false,
                .arena = arena,
            };
        }

        pub fn deinit(self: *Function) void {
            self.arena.deinit();
        }

        pub fn inspect(self: *const Object.Function, writer: *std.Io.Writer) !void {
            try writer.writeAll("fn(");
            for (self.parameters, 0..) |parameter, i| {
                try parameter.toString(writer);
                if (i < self.parameters.len - 1) {
                    try writer.writeAll(", ");
                }
            }

            try writer.writeAll(") {");
            try self.body.toString(writer);
            try writer.writeAll("}");
            try writer.flush();
        }
    };

    pub const Array = struct {
        elements: []Object,
        marked: bool = false,
        ref_count: usize = 0,

        pub fn init(elements: []Object) Array {
            return .{
                .elements = elements,
            };
        }

        pub fn inspect(self: *const Object.Array, writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("[");
            for (self.elements, 0..) |*elem, i| {
                try elem.inspect(writer);
                if (i < self.elements.len - 1) try writer.writeAll(", ");
            }
            try writer.writeAll("]");
            try writer.flush();
        }
    };

    pub const HashPair = struct {
        key: Object,
        value: Object,
    };

    pub const Hash = struct {
        pairs: std.AutoHashMap(HashKey, HashPair),
        marked: bool = false,
        ref_count: usize = 0,

        pub fn inspect(self: *const Object.Hash, writer: *std.Io.Writer) std.Io.Writer.Error!void {
            try writer.writeAll("{");
            var iterator = self.pairs.iterator();
            var i: usize = 0;
            while (iterator.next()) |entry| {
                try entry.value_ptr.key.inspect(writer);
                try writer.writeAll(": ");
                try entry.value_ptr.value.inspect(writer);
                if (i < self.pairs.count() - 1) try writer.writeAll(", ");
                i += 1;
            }
            try writer.writeAll("}");
            try writer.flush();
        }
    };

    pub fn hashKey(self: *const Self) ?HashKey {
        return switch (self.*) {
            .int => |obj| obj.hashKey(),
            .bool => |obj| obj.hashKey(),
            .string => |obj| obj.hashKey(),
            else => null,
        };
    }

    pub fn typeName(self: *const Self) []const u8 {
        return @tagName(self.*);
    }

    pub fn inspect(self: *const Self, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
            inline else => |obj| {
                try obj.inspect(writer);
            },
        }
    }
};
