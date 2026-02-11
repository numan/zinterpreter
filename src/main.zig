const std = @import("std");
const lib = @import("zinterpreter_lib");
const repl = lib.repl;

pub fn main() !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Hello! This is the monkey programming language!\n", .{});
    try stdout.print("This is a simple interpreter for the monkey programming language.\n", .{});
    try stdout.print("Try to type in commands \n", .{});
    try stdout.flush();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    try repl.run(allocator);
}
