//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;
pub const lexer = @import("lib/lexer.zig");
pub const parser = @import("lib/parser.zig");
pub const repl = @import("repl/repl.zig");

comptime {
    _ = @import("lib/lexer.zig");
    _ = @import("lib/parsing_utils.zig");
    _ = @import("lib/parser.zig");
    _ = @import("lib/object.zig");
    _ = @import("lib/evaluator.zig");
}

// All the tests go here
comptime {
    _ = @import("lib/parser_test.zig");
    _ = @import("lib/evaluator_test.zig");
}
