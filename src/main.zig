const std = @import("std");
const lib = @import("zinterpreter_lib");
const repl = lib.repl;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Hello! This is the monkey programming language!\n", .{});
    try stdout.print("This is a simple interpreter for the monkey programming language.\n", .{});
    try stdout.print("Try to type in commands \n", .{});
    try bw.flush();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    try repl.run(allocator);
}
