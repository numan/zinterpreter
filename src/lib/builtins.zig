const std = @import("std");
const object = @import("object.zig");

const Object = object.Object;
const BuiltinError = object.BuiltinError;

pub const Builtin = enum {
    len,
    puts,
    first,
    last,
    rest,
    push,
};

const definitions = std.enums.EnumArray(Builtin, Object.Builtin).init(.{
    .len = .{ .function = &builtinLen },
    .puts = .{ .writer_function = &builtinPuts },
    .first = .{ .function = &builtinFirst },
    .last = .{ .function = &builtinLast },
    .rest = .{ .function = &builtinRest },
    .push = .{ .function = &builtinPush },
});

pub fn fromName(name: []const u8) ?Builtin {
    return std.meta.stringToEnum(Builtin, name);
}

pub fn getObject(b: Builtin) Object {
    return .{ .builtin = definitions.get(b) };
}

pub fn getBuiltinByName(name: []const u8) ?Object {
    const b = fromName(name) orelse return null;
    return getObject(b);
}

fn newError(allocator: std.mem.Allocator, comptime format: []const u8, args: anytype) !Object {
    const msg = try std.fmt.allocPrint(allocator, format, args);
    return .{ .err = Object.Error.init(msg) };
}

fn builtinLen(allocator: std.mem.Allocator, args: []const Object) std.mem.Allocator.Error!Object {
    if (args.len != 1) {
        return try newError(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .string => |s| .{ .int = Object.Integer.init(@intCast(s.value.len)) },
        .array => |a| .{ .int = Object.Integer.init(@intCast(a.elements.len)) },
        else => return try newError(allocator, "argument to `len` not supported, got {s}", .{args[0].typeName()}),
    };
}

fn builtinPuts(_: std.mem.Allocator, args: []const Object, writer: *std.Io.Writer) BuiltinError!Object {
    for (args) |*arg| {
        try printObject(arg, writer);
        try writer.writeAll("\n");
    }
    return .{ .null = Object.Null.init() };
}

fn printObject(obj: *const Object, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (obj.*) {
        .int => |i| try writer.print("{d}", .{i.value}),
        .bool => |b| try writer.print("{}", .{b.value}),
        .null => try writer.writeAll("null"),
        .string => |s| try writer.writeAll(s.value),
        .err => |e| {
            try writer.writeAll("ERROR: ");
            try writer.writeAll(e.msg);
        },
        .array => |a| {
            try writer.writeAll("[");
            for (a.elements, 0..) |*elem, i| {
                try printObject(elem, writer);
                if (i < a.elements.len - 1) try writer.writeAll(", ");
            }
            try writer.writeAll("]");
        },
        .function => try writer.writeAll("fn(...) {...}"),
        .compiled_function => try writer.writeAll("CompiledFunction"),
        .builtin => try writer.writeAll("builtin function"),
        .hash => try writer.writeAll("{...}"),
    }
}

fn builtinFirst(allocator: std.mem.Allocator, args: []const Object) std.mem.Allocator.Error!Object {
    if (args.len != 1) {
        return try newError(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .array => |arr| {
            if (arr.elements.len > 0) {
                return arr.elements[0];
            }
            return .{ .null = Object.Null.init() };
        },
        else => try newError(allocator, "argument to `first` must be ARRAY, got {s}", .{args[0].typeName()}),
    };
}

fn builtinLast(allocator: std.mem.Allocator, args: []const Object) std.mem.Allocator.Error!Object {
    if (args.len != 1) {
        return try newError(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .array => |arr| {
            if (arr.elements.len > 0) {
                return arr.elements[arr.elements.len - 1];
            }
            return .{ .null = Object.Null.init() };
        },
        else => try newError(allocator, "argument to `last` must be ARRAY, got {s}", .{args[0].typeName()}),
    };
}

fn builtinRest(allocator: std.mem.Allocator, args: []const Object) std.mem.Allocator.Error!Object {
    if (args.len != 1) {
        return try newError(allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .array => |arr| {
            if (arr.elements.len == 0) {
                return .{ .null = Object.Null.init() };
            }
            const new_elements = try allocator.alloc(Object, arr.elements.len - 1);
            @memcpy(new_elements, arr.elements[1..]);
            const array_ptr = try allocator.create(Object.Array);
            array_ptr.* = Object.Array.init(new_elements);
            return .{ .array = array_ptr };
        },
        else => try newError(allocator, "argument to `rest` must be ARRAY, got {s}", .{args[0].typeName()}),
    };
}

fn builtinPush(allocator: std.mem.Allocator, args: []const Object) std.mem.Allocator.Error!Object {
    if (args.len != 2) {
        return try newError(allocator, "wrong number of arguments. got={d}, want=2", .{args.len});
    }
    return switch (args[0]) {
        .array => |arr| {
            const new_elements = try allocator.alloc(Object, arr.elements.len + 1);
            @memcpy(new_elements[0..arr.elements.len], arr.elements);
            new_elements[arr.elements.len] = args[1];
            const array_ptr = try allocator.create(Object.Array);
            array_ptr.* = Object.Array.init(new_elements);
            return .{ .array = array_ptr };
        },
        else => try newError(allocator, "argument to `push` must be ARRAY, got {s}", .{args[0].typeName()}),
    };
}
