const std = @import("std");
const Lexer = @import("../lib/lexer.zig").Lexer;

const PROMPT = ">> ";
pub fn run(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("{s} ", .{PROMPT});
    try bw.flush();

    const input = try stdin.readUntilDelimiterAlloc(allocator, '\n', 10_000);
    var lexer = Lexer.init(input);

    var token = lexer.nextToken();
    try stdout.print("\n", .{});
    while (token.token_type != .eof) : (token = lexer.nextToken()) {
        try stdout.print("Type: {any} Value: {s}\n", .{ token.token_type, token.ch });
        try bw.flush();
    }
}
