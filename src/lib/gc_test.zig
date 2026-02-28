const std = @import("std");
const testing = std.testing;

const Object = @import("object.zig").Object;
const Gc = @import("gc.zig").Gc;
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Evaluator = @import("evaluator.zig").Evaluator;

fn evalWithGc(input: []const u8, allocator: std.mem.Allocator, evaluator: *Evaluator) !Object {
    var lexer = Lexer.init(input);
    var parser = Parser.init(allocator, &lexer);

    const program = try parser.parse();
    if (parser.allErrors().len != 0) {
        return error.TestUnexpectedResult;
    }

    return evaluator.eval(program);
}

test "gc collects unreachable environments" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    _ = try collector.allocEnvironment(global);

    try testing.expectEqual(2, collector.trackedEnvironmentCount());

    collector.collect(global);

    try testing.expectEqual(1, collector.trackedEnvironmentCount());
}

test "gc keeps closure captured environment alive" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const captured = try collector.allocEnvironment(global);

    const closure: Object = try collector.allocFunction(.{
        .parameters = &.{},
        .body = undefined,
        .environment = captured,
        .arena = std.heap.ArenaAllocator.init(testing.allocator),
    });

    _ = try collector.envSet(global, "keep", closure);

    collector.collect(global);

    try testing.expectEqual(2, collector.trackedEnvironmentCount());
    try testing.expectEqual(1, collector.trackedFunctionCount());
}

test "gc collects unreachable environment function cycle" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const cycle_env = try collector.allocEnvironment(null);

    const closure: Object = try collector.allocFunction(.{
        .parameters = &.{},
        .body = undefined,
        .environment = cycle_env,
        .arena = std.heap.ArenaAllocator.init(testing.allocator),
    });

    _ = try collector.envSet(cycle_env, "self", closure);

    try testing.expectEqual(2, collector.trackedEnvironmentCount());
    try testing.expectEqual(1, collector.trackedFunctionCount());

    collector.collect(global);

    try testing.expectEqual(1, collector.trackedEnvironmentCount());
    try testing.expectEqual(0, collector.trackedFunctionCount());
}

test "gc collects unreachable strings" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    _ = try collector.allocString("hello");

    try testing.expectEqual(1, collector.trackedStringCount());

    collector.collect(global);

    try testing.expectEqual(0, collector.trackedStringCount());
}

test "gc keeps reachable strings" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const str_obj = try collector.allocString("hello");
    _ = try collector.envSet(global, "greeting", str_obj);

    collector.collect(global);

    try testing.expectEqual(1, collector.trackedStringCount());
}

test "gc integration collects transient call frame" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    var evaluator = Evaluator.init(global, &collector);
    defer evaluator.deinit();

    _ = try evalWithGc(
        "let identity = fn(x) { x; }; identity(5);",
        arena.allocator(),
        &evaluator,
    );

    try testing.expectEqual(2, collector.trackedEnvironmentCount());
    try testing.expectEqual(1, collector.trackedFunctionCount());

    collector.collect(global);

    try testing.expectEqual(1, collector.trackedEnvironmentCount());
    try testing.expectEqual(1, collector.trackedFunctionCount());
}

test "gc integration keeps escaping closure then reclaims it" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    var evaluator = Evaluator.init(global, &collector);
    defer evaluator.deinit();

    _ = try evalWithGc(
        "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2);",
        arena.allocator(),
        &evaluator,
    );

    collector.collect(global);

    try testing.expectEqual(2, collector.trackedEnvironmentCount());
    try testing.expectEqual(2, collector.trackedFunctionCount());

    _ = try evalWithGc("let addTwo = 0;", arena.allocator(), &evaluator);
    collector.collect(global);

    try testing.expectEqual(1, collector.trackedEnvironmentCount());
    try testing.expectEqual(1, collector.trackedFunctionCount());
}

test "rc overwrite releases old value" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const str_obj = try collector.allocString("hello");

    // Store string in env — retains it (ref_count = 1)
    _ = try collector.envSet(global, "x", str_obj);
    try testing.expectEqual(1, collector.trackedStringCount());

    // Overwrite with int — releases string (ref_count -> 0), freed immediately
    _ = try collector.envSet(global, "x", .{ .int = Object.Integer.init(42) });
    try testing.expectEqual(0, collector.trackedStringCount());
}

test "rc cascading free through environment" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    _ = try collector.allocEnvironment(null);
    const inner = try collector.allocEnvironment(null);

    const str_obj = try collector.allocString("cascade");
    _ = try collector.envSet(inner, "val", str_obj);

    try testing.expectEqual(1, collector.trackedStringCount());
    try testing.expectEqual(2, collector.trackedEnvironmentCount());

    // Manually retain then release inner env to trigger cascading free
    collector.retainEnvironment(inner);
    try testing.expectEqual(1, inner.ref_count);

    collector.releaseEnvironment(inner);
    // inner freed → string inside freed too
    try testing.expectEqual(0, collector.trackedStringCount());
    try testing.expectEqual(1, collector.trackedEnvironmentCount());
}

test "rc multiple references freed only when last ref drops" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const str_obj = try collector.allocString("shared");

    // Store under two keys — ref_count = 2
    _ = try collector.envSet(global, "a", str_obj);
    _ = try collector.envSet(global, "b", str_obj);
    try testing.expectEqual(1, collector.trackedStringCount());
    try testing.expectEqual(2, str_obj.string.ref_count);

    // Overwrite one key — ref_count = 1, still alive
    _ = try collector.envSet(global, "a", .{ .int = Object.Integer.init(1) });
    try testing.expectEqual(1, collector.trackedStringCount());
    try testing.expectEqual(1, str_obj.string.ref_count);

    // Overwrite second key — ref_count = 0, freed
    _ = try collector.envSet(global, "b", .{ .int = Object.Integer.init(2) });
    try testing.expectEqual(0, collector.trackedStringCount());
}

test "rc cycle not collected by rc alone but collected by mark-and-sweep" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const cycle_env = try collector.allocEnvironment(null);

    const closure: Object = try collector.allocFunction(.{
        .parameters = &.{},
        .body = undefined,
        .environment = cycle_env,
        .arena = std.heap.ArenaAllocator.init(testing.allocator),
    });

    // Create cycle: cycle_env stores closure, closure captures cycle_env
    _ = try collector.envSet(cycle_env, "self", closure);

    // Both are still tracked (cycle keeps ref_counts >= 1)
    try testing.expectEqual(2, collector.trackedEnvironmentCount());
    try testing.expectEqual(1, collector.trackedFunctionCount());

    // Mark-and-sweep from global collects the cycle
    collector.collect(global);
    try testing.expectEqual(1, collector.trackedEnvironmentCount());
    try testing.expectEqual(0, collector.trackedFunctionCount());
}

test "allocStringConcat tracks and produces correct value" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const result = try collector.allocStringConcat("hello", " world");
    try testing.expectEqualStrings("hello world", result.string.value);
    try testing.expectEqual(1, collector.trackedStringCount());
}

test "allocStringConcat with empty strings" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const left_empty = try collector.allocStringConcat("", "world");
    try testing.expectEqualStrings("world", left_empty.string.value);

    const right_empty = try collector.allocStringConcat("hello", "");
    try testing.expectEqualStrings("hello", right_empty.string.value);

    const both_empty = try collector.allocStringConcat("", "");
    try testing.expectEqualStrings("", both_empty.string.value);

    try testing.expectEqual(3, collector.trackedStringCount());
}

test "allocStringConcat result freed on env overwrite" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const env = try collector.allocEnvironment(null);
    const concat_obj = try collector.allocStringConcat("foo", "bar");
    _ = try collector.envSet(env, "x", concat_obj);
    try testing.expectEqual(1, collector.trackedStringCount());

    // Overwrite with int — RC drops to 0, string freed immediately
    _ = try collector.envSet(env, "x", .{ .int = Object.Integer.init(42) });
    try testing.expectEqual(0, collector.trackedStringCount());
}

test "rc integration overwrite triggers immediate free" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    var evaluator = Evaluator.init(global, &collector);
    defer evaluator.deinit();

    _ = try evalWithGc("let x = \"hello\";", arena.allocator(), &evaluator);
    try testing.expectEqual(1, collector.trackedStringCount());

    // Overwrite x — string freed immediately via RC, no collect() needed
    _ = try evalWithGc("let x = 42;", arena.allocator(), &evaluator);
    try testing.expectEqual(0, collector.trackedStringCount());
}

test "rc reassignment releases old value" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    var evaluator = Evaluator.init(global, &collector);
    defer evaluator.deinit();

    // x holds "foo", y copies the reference — string ref_count = 2
    _ = try evalWithGc("let x = \"foo\"; let y = x;", arena.allocator(), &evaluator);
    try testing.expectEqual(1, collector.trackedStringCount());

    // Reassign x to int — releases one ref, string still alive via y
    _ = try evalWithGc("x = 10;", arena.allocator(), &evaluator);
    try testing.expectEqual(1, collector.trackedStringCount());

    // Reassign y to int — releases last ref, string freed
    _ = try evalWithGc("y = 20;", arena.allocator(), &evaluator);
    try testing.expectEqual(0, collector.trackedStringCount());
}
