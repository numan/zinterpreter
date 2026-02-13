const std = @import("std");

pub const Object = union(enum) {
    int: Integer,

    const Self = @This();

    pub const Integer = struct {
        value: i64,

        pub fn init(value: i64) Integer {
            return Integer{
                .value = value,
            };
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

        pub fn inspect(self: *const Object.Boolean, writer: *std.Io.Writer) !void {
            try writer.print("{d}", .{self.value});
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
            try writer.printAll("null");
            try writer.flush();
        }
    };

    pub fn inspect(self: *const Self, writer: *std.Io.Writer) !void {
        switch (self.*) {
            inline else => |*obj| {
                try obj.*.inspect(writer);
            },
        }
    }
};
