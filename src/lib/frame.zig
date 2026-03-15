const object = @import("object.zig");
const code = @import("code.zig");

const Object = object.Object;

pub const Frame = struct {
    function: *const Object.CompiledFunction,
    ip: usize,
    base_pointer: usize,

    pub fn init(function: *const Object.CompiledFunction, base_pointer: usize) Frame {
        return .{
            .function = function,
            .ip = 0,
            .base_pointer = base_pointer,
        };
    }

    pub fn instructions(self: *const Frame) []const u8 {
        return self.function.instructions;
    }
};
