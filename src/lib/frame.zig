const object = @import("object.zig");
const code = @import("code.zig");

const Object = object.Object;

pub const Frame = struct {
    function: *const Object.CompiledFunction,
    ip: usize,

    pub fn init(function: *const Object.CompiledFunction) Frame {
        return .{
            .function = function,
            .ip = 0,
        };
    }

    pub fn instructions(self: *const Frame) []const u8 {
        return self.function.instructions;
    }
};
