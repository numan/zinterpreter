const std = @import("std");
const object = @import("object.zig");
const gc = @import("gc.zig");
const Object = object.Object;
const Gc = gc.Gc;

const BuiltinMapType = std.StaticStringMap(Object);
pub const builtins = BuiltinMapType.initComptime(.{
    .{
        "len", Object{ .builtin = Object.Builtin.init(&len) },
    },
});

fn len(collector: *Gc, args: []const Object) std.mem.Allocator.Error!Object {
    if (args.len != 1) {
        const msg = try std.fmt.allocPrint(collector.allocator, "wrong number of arguments. got={d}, want=1", .{args.len});
        return try collector.allocErrorOwned(msg);
    }
    return switch (args[0]) {
        .string => |s| .{ .int = Object.Integer.init(@intCast(s.value.len)) },
        else => {
            const msg = try std.fmt.allocPrint(collector.allocator, "argument to `len` not supported, got {s}", .{args[0].typeName()});
            return try collector.allocErrorOwned(msg);
        },
    };
}
