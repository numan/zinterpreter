const std = @import("std");
const lib = @import("zinterpreter_lib");
const repl = lib.repl;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    try repl.run(allocator);
}
