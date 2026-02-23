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

    try testing.expectEqual(@as(usize, 2), collector.trackedEnvironmentCount());

    collector.collect(global);

    try testing.expectEqual(@as(usize, 1), collector.trackedEnvironmentCount());
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
    });

    _ = try global.set("keep", closure);

    collector.collect(global);

    try testing.expectEqual(@as(usize, 2), collector.trackedEnvironmentCount());
    try testing.expectEqual(@as(usize, 1), collector.trackedFunctionCount());
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
    });

    _ = try cycle_env.set("self", closure);

    try testing.expectEqual(@as(usize, 2), collector.trackedEnvironmentCount());
    try testing.expectEqual(@as(usize, 1), collector.trackedFunctionCount());

    collector.collect(global);

    try testing.expectEqual(@as(usize, 1), collector.trackedEnvironmentCount());
    try testing.expectEqual(@as(usize, 0), collector.trackedFunctionCount());
}

test "gc collects unreachable errors" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const msg = try testing.allocator.dupe(u8, "boom");
    _ = try collector.allocErrorOwned(msg);

    try testing.expectEqual(@as(usize, 1), collector.trackedErrorCount());

    collector.collect(global);

    try testing.expectEqual(@as(usize, 0), collector.trackedErrorCount());
}

test "gc keeps reachable errors" {
    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    const msg = try testing.allocator.dupe(u8, "boom");
    const err_obj = try collector.allocErrorOwned(msg);
    _ = try global.set("last_error", err_obj);

    collector.collect(global);

    try testing.expectEqual(@as(usize, 1), collector.trackedErrorCount());
}

test "gc integration collects transient call frame" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    var evaluator = Evaluator.initWithGc(arena.allocator(), global, &collector);

    _ = try evalWithGc(
        "let identity = fn(x) { x; }; identity(5);",
        arena.allocator(),
        &evaluator,
    );

    try testing.expectEqual(@as(usize, 2), collector.trackedEnvironmentCount());
    try testing.expectEqual(@as(usize, 1), collector.trackedFunctionCount());

    collector.collect(global);

    try testing.expectEqual(@as(usize, 1), collector.trackedEnvironmentCount());
    try testing.expectEqual(@as(usize, 1), collector.trackedFunctionCount());
}

test "gc integration keeps escaping closure then reclaims it" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    var evaluator = Evaluator.initWithGc(arena.allocator(), global, &collector);

    _ = try evalWithGc(
        "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2);",
        arena.allocator(),
        &evaluator,
    );

    collector.collect(global);

    try testing.expectEqual(@as(usize, 2), collector.trackedEnvironmentCount());
    try testing.expectEqual(@as(usize, 2), collector.trackedFunctionCount());

    _ = try evalWithGc("let addTwo = 0;", arena.allocator(), &evaluator);
    collector.collect(global);

    try testing.expectEqual(@as(usize, 1), collector.trackedEnvironmentCount());
    try testing.expectEqual(@as(usize, 1), collector.trackedFunctionCount());
}

test "gc integration reclaims temporary error objects" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var collector = Gc.init(testing.allocator);
    defer collector.deinit();

    const global = try collector.allocEnvironment(null);
    var evaluator = Evaluator.initWithGc(arena.allocator(), global, &collector);

    const result = try evalWithGc("foobar", arena.allocator(), &evaluator);
    switch (result) {
        .err => {},
        else => return error.TestUnexpectedResult,
    }

    try testing.expectEqual(@as(usize, 1), collector.trackedErrorCount());

    collector.collect(global);

    try testing.expectEqual(@as(usize, 0), collector.trackedErrorCount());
}
