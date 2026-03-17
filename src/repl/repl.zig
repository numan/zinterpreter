const std = @import("std");
const Lexer = @import("../lib/lexer.zig").Lexer;
const Parser = @import("../lib/parser.zig").Parser;
const EvalState = @import("../lib/eval_state.zig").EvalState;
const repl_utils = @import("repl_utils.zig");

const PROMPT = repl_utils.PROMPT;
pub fn run(io: std.Io, allocator: std.mem.Allocator) !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().reader(io, &stdin_buffer);
    const stdin = &stdin_reader.interface;

    var state = try EvalState.init(allocator);
    defer state.deinit();

    var evaluator = state.newEvaluator(stdout);
    defer evaluator.deinit();

    try stdout.print("{s} ", .{PROMPT});
    try stdout.flush();

    while (stdin.takeDelimiterExclusive('\n')) |line| {
        stdin.toss(1);

        var parser_arena = std.heap.ArenaAllocator.init(allocator);
        defer parser_arena.deinit();

        var lexer = Lexer.init(line);
        var parser = Parser.init(parser_arena.allocator(), &lexer);

        const program = try parser.parse();
        const errors = parser.allErrors();

        if (errors.len != 0) {
            try repl_utils.printParseErrors(errors, stdout);
        } else {
            const eval = try evaluator.eval(program);
            try eval.inspect(stdout);
            try stdout.writeAll("\n");
            state.collect();
        }

        try stdout.print("{s} ", .{PROMPT});

        try stdout.flush();
    } else |err| {
        switch (err) {
            error.EndOfStream => {
                try stdout.writeAll("Bye!\n");
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
