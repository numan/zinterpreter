const std = @import("std");
const Lexer = @import("../lib/lexer.zig").Lexer;
const Parser = @import("../lib/parser.zig").Parser;
const Evaluator = @import("../lib/evaluator.zig").Evaluator;
const Gc = @import("../lib/gc.zig").Gc;
const Compiler = @import("../lib/compiler.zig").Compiler;
const vm_mod = @import("../lib/vm.zig");
const Vm = vm_mod.Vm;
const Object = @import("../lib/object.zig").Object;
const SymbolTable = @import("../lib/symbol_table.zig").SymbolTable;

const PROMPT = ">> ";
const MONKEY_FACE =
    \\            __,__
    \\   .--.  .-"     "-.  .--.
    \\  / .. \/  .-. .-.  \/ .. \
    \\ | |  '|  /   Y   \  |'  | |
    \\ | \   \  \ 0 | 0 /  /   / |
    \\ \ '- ,\.-"""""""-./, -' /
    \\   ''-' /_   ^ ^   _\ '-''
    \\       |  \._   _./  |
    \\       \   \ '~' /   /
    \\        '._ '-=-' _.'
    \\           '-----'
    \\
;
pub fn run(io: std.Io, allocator: std.mem.Allocator) !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().reader(io, &stdin_buffer);
    const stdin = &stdin_reader.interface;

    // Persistent state across REPL iterations
    var st_arena = std.heap.ArenaAllocator.init(allocator);
    defer st_arena.deinit();
    var symbol_table = SymbolTable.init(st_arena.allocator());
    defer symbol_table.deinit();

    var constants = std.ArrayList(Object).empty;
    defer constants.deinit(allocator);

    var globals: [vm_mod.globals_size]Object = undefined;

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
            try printParseErrors(errors, stdout);
        } else {
            var compiler = Compiler.init(allocator, &symbol_table, &constants, allocator);
            defer compiler.deinit();

            compiler.compile(program) catch |err| {
                try stdout.print("Compiler error: {s}\n", .{@errorName(err)});
                try stdout.print("{s} ", .{PROMPT});
                try stdout.flush();
                continue;
            };

            const bc = compiler.bytecode();

            var vm = Vm.init(bc, &globals);
            vm.run() catch |err| {
                try stdout.print("VM error: {s}\n", .{@errorName(err)});
                try stdout.print("{s} ", .{PROMPT});
                try stdout.flush();
                continue;
            };

            const stack_top = vm.lastPoppedStackElem() orelse {
                try stdout.writeAll("null");
                try stdout.writeAll("\n");
                continue;
            };
            try stack_top.inspect(stdout);
            try stdout.writeAll("\n");
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

pub fn printParseErrors(errors: [][]const u8, writer: *std.Io.Writer) !void {
    try writer.writeAll(MONKEY_FACE);
    try writer.writeAll("\n");
    try writer.writeAll("Whoops! We ran into some monkey business here!\n");
    try writer.writeAll(" Parser Errors:\n");

    for (errors) |e| {
        try writer.print("\t{s}\n", .{e});
    }
    try writer.flush();
}
