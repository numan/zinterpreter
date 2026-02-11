const std = @import("std");
const Lexer = @import("../lib/lexer.zig").Lexer;
const Parser = @import("../lib/parser.zig").Parser;

const PROMPT = ">> ";
pub fn run(allocator: std.mem.Allocator) !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin = &stdin_reader.interface;

    try stdout.print("{s} ", .{PROMPT});
    try stdout.flush();

    while (stdin.takeDelimiterExclusive('\n')) |line| {
        stdin.toss(1);

        var lexer = Lexer.init(line);
        var parser = Parser.init(allocator, &lexer);
        defer parser.deinit();

        const program = try parser.parse();
        const errors = parser.allErrors();

        if (errors.len != 0) {
            try printParseErrors(errors, stdout);
            continue;
        }

        try program.toString(stdout);
        try stdout.writeAll("\n");
        try stdout.flush();
    } else |err| {
        switch (err) {
            error.EndOfStream => {
                try stdout.writeAll("Bye!");
            },
            error.StreamTooLong => {
                return err;
            },
            error.ReadFailed => {
                return err;
            },
        }

        try stdout.flush();
    }
}

pub fn printParseErrors(errors: [][]const u8, writer: *std.Io.Writer) !void {
    for (errors) |e| {
        try writer.print("Found Error: {s}\n", .{e});
    }
    try writer.flush();
}
