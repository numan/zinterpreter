const std = @import("std");
const lib = @import("zinterpreter_lib");

const Lexer = lib.lexer.Lexer;
const Parser = lib.parser.Parser;
const Compiler = lib.compiler.Compiler;
const Vm = lib.vm.Vm;
const Evaluator = lib.evaluator.Evaluator;
const Gc = lib.gc.Gc;
const Object = lib.object.Object;
const SymbolTable = lib.symbol_table.SymbolTable;

const input =
    \\let fibonacci = fn(x) {
    \\    if (x < 2) {
    \\        return x;
    \\    } else {
    \\        fibonacci(x - 1) + fibonacci(x - 2);
    \\    }
    \\};
    \\let loop = fn(i) {
    \\    if (i > 0) {
    \\        puts(fibonacci(28));
    \\        loop(i - 1);
    \\    }
    \\};
    \\loop(5);
;

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.Io.File.stdout().writer(init.io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Parse engine flag from args
    var args = std.process.Args.Iterator.init(init.minimal.args);
    _ = args.skip(); // skip program name
    var engine: []const u8 = "vm";
    while (args.next()) |arg| {
        if (std.mem.startsWith(u8, arg, "-engine=")) {
            engine = arg["-engine=".len..];
        }
    }

    // Parse the program
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);
    defer parser.deinit();
    const program = try parser.parse();

    if (std.mem.eql(u8, engine, "vm")) {
        // Compile
        var symbol_table = SymbolTable.init(allocator, null);
        defer symbol_table.deinit();
        var constants = std.ArrayList(Object).empty;
        defer constants.deinit(allocator);
        var comp = Compiler.init(allocator, &symbol_table, &constants);
        defer comp.deinit();
        try comp.enterScope();
        try comp.compile(program);

        var vm_arena = std.heap.ArenaAllocator.init(allocator);
        defer vm_arena.deinit();
        var globals: [lib.vm.globals_size]Object = undefined;
        var vm = try Vm.init(comp.bytecode(), &globals, vm_arena.allocator(), stdout);

        // Time only the VM execution
        const start = std.Io.Clock.Timestamp.now(init.io, .awake);
        try vm.run();
        const duration = start.untilNow(init.io);

        try stdout.print("engine=vm, duration=", .{});
        try duration.raw.format(stdout);
        try stdout.print("\n", .{});
    } else {
        // Eval path
        var collector = Gc.init(allocator);
        defer collector.deinit();
        const env = try collector.allocEnvironment(null);
        var evaluator = Evaluator.init(env, &collector, stdout);
        defer evaluator.deinit();

        // Time only the evaluation
        const start = std.Io.Clock.Timestamp.now(init.io, .awake);
        _ = try evaluator.eval(program);
        const duration = start.untilNow(init.io);

        try stdout.print("engine=eval, duration=", .{});
        try duration.raw.format(stdout);
        try stdout.print("\n", .{});
    }

    try stdout.flush();
}
