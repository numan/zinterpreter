const std = @import("std");
const object = @import("object.zig");
const Object = object.Object;

test "same int values produce same hash key" {
    const a = Object{ .int = Object.Integer.init(42) };
    const b = Object{ .int = Object.Integer.init(42) };
    try std.testing.expectEqual(a.hashKey().?, b.hashKey().?);
}

test "different int values produce different hash keys" {
    const a = Object{ .int = Object.Integer.init(1) };
    const b = Object{ .int = Object.Integer.init(2) };
    try std.testing.expect(!std.meta.eql(a.hashKey().?, b.hashKey().?));
}

test "same bool values produce same hash key" {
    const a = Object{ .bool = Object.Boolean.init(true) };
    const b = Object{ .bool = Object.Boolean.init(true) };
    try std.testing.expectEqual(a.hashKey().?, b.hashKey().?);
}

test "different bool values produce different hash keys" {
    const a = Object{ .bool = Object.Boolean.init(true) };
    const b = Object{ .bool = Object.Boolean.init(false) };
    try std.testing.expect(!std.meta.eql(a.hashKey().?, b.hashKey().?));
}

test "same string content produces same hash key" {
    var s1 = Object.String.init("hello");
    var s2 = Object.String.init("hello");
    const a = Object{ .string = &s1 };
    const b = Object{ .string = &s2 };
    try std.testing.expectEqual(a.hashKey().?, b.hashKey().?);
}

test "different string content produces different hash keys" {
    var s1 = Object.String.init("hello");
    var s2 = Object.String.init("world");
    const a = Object{ .string = &s1 };
    const b = Object{ .string = &s2 };
    try std.testing.expect(!std.meta.eql(a.hashKey().?, b.hashKey().?));
}

test "int(1) and bool(true) produce different hash keys (cross-type)" {
    const a = Object{ .int = Object.Integer.init(1) };
    const b = Object{ .bool = Object.Boolean.init(true) };
    try std.testing.expect(!std.meta.eql(a.hashKey().?, b.hashKey().?));
}

test "non-hashable types return null" {
    const null_obj = Object{ .null = Object.Null.init() };
    const err_obj = Object{ .err = Object.Error.init("some error") };
    try std.testing.expectEqual(null, null_obj.hashKey());
    try std.testing.expectEqual(null, err_obj.hashKey());
}
