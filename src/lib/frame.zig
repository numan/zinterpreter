const object = @import("object.zig");
const code = @import("code.zig");

const Object = object.Object;

pub const Frame = struct {
    closure: *const Object.Closure,
    ip: usize,
    base_pointer: usize,
    ins: []const u8,

    pub fn init(function: *const Object.Closure, base_pointer: usize) Frame {
        return .{
            .closure = function,
            .ip = 0,
            .base_pointer = base_pointer,
            .ins = function.function.*.instructions,
        };
    }

    pub fn instructions(self: *const Frame) []const u8 {
        return self.ins;
    }
};
