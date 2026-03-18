//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const testing = std.testing;
pub const lexer = @import("lib/lexer.zig");
pub const parser = @import("lib/parser.zig");
pub const compiler = @import("lib/compiler.zig");
pub const vm = @import("lib/vm.zig");
pub const evaluator = @import("lib/evaluator.zig");
pub const gc = @import("lib/gc.zig");
pub const object = @import("lib/object.zig");
pub const symbol_table = @import("lib/symbol_table.zig");
pub const repl = @import("repl/vm_repl.zig");

comptime {
    _ = @import("lib/lexer.zig");
    _ = @import("lib/parsing_utils.zig");
    _ = @import("lib/parser.zig");
    _ = @import("lib/object.zig");
    _ = @import("lib/builtins.zig");
    _ = @import("lib/evaluator.zig");
    _ = @import("lib/gc.zig");
    _ = @import("lib/code.zig");
    _ = @import("lib/vm.zig");
    _ = @import("lib/vm_state.zig");
    _ = @import("lib/eval_state.zig");
}

// All the tests go here
comptime {
    _ = @import("lib/parser_test.zig");
    _ = @import("lib/evaluator_test.zig");
    _ = @import("lib/gc_test.zig");
    _ = @import("lib/object_test.zig");
    _ = @import("lib/code_test.zig");
    _ = @import("lib/compiler_test.zig");
    _ = @import("lib/vm_test.zig");
    _ = @import("lib/symbol_table_test.zig");
}
