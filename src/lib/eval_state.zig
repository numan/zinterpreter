const std = @import("std");
const Gc = @import("gc.zig").Gc;
const Evaluator = @import("evaluator.zig").Evaluator;
const Environment = @import("environment.zig").Environment;

pub const EvalState = struct {
    gc: Gc,
    environment: *Environment,

    pub fn init(allocator: std.mem.Allocator) !EvalState {
        var gc = Gc.init(allocator);
        const env = try gc.allocEnvironment(null);
        return .{
            .gc = gc,
            .environment = env,
        };
    }

    pub fn deinit(self: *EvalState) void {
        self.gc.deinit();
    }

    pub fn newEvaluator(self: *EvalState, writer: *std.Io.Writer) Evaluator {
        return Evaluator.init(self.environment, &self.gc, writer);
    }

    pub fn collect(self: *EvalState) void {
        self.gc.collect(self.environment);
    }
};
