const std = @import("std");
const compiler_mod = @import("compiler.zig");
const Compiler = compiler_mod.Compiler;
const Bytecode = compiler_mod.Bytecode;
const vm_mod = @import("vm.zig");
const Vm = vm_mod.Vm;
const Object = @import("object.zig").Object;
const SymbolTable = @import("symbol_table.zig").SymbolTable;

pub const VmState = struct {
    allocator: std.mem.Allocator,
    st_arena: std.heap.ArenaAllocator,
    vm_arena: std.heap.ArenaAllocator,
    symbol_table: SymbolTable,
    constants: std.ArrayList(Object),
    globals: [vm_mod.globals_size]Object,

    pub fn init(allocator: std.mem.Allocator) VmState {
        var st_arena = std.heap.ArenaAllocator.init(allocator);
        return .{
            .allocator = allocator,
            .st_arena = st_arena,
            .vm_arena = std.heap.ArenaAllocator.init(allocator),
            .symbol_table = SymbolTable.init(st_arena.allocator()),
            .constants = .empty,
            .globals = undefined,
        };
    }

    pub fn deinit(self: *VmState) void {
        self.constants.deinit(self.allocator);
        self.symbol_table.deinit();
        self.st_arena.deinit();
        self.vm_arena.deinit();
    }

    pub fn newCompiler(self: *VmState) !Compiler {
        var comp = Compiler.init(self.allocator, &self.symbol_table, &self.constants, self.allocator);
        try comp.enterScope();
        return comp;
    }

    pub fn newVm(self: *VmState, bytecode: Bytecode) Vm {
        return Vm.init(bytecode, &self.globals, self.vm_arena.allocator());
    }
};
