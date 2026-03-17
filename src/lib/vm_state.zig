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
    vm_arena: std.heap.ArenaAllocator,
    symbol_table: SymbolTable,
    constants: std.ArrayList(Object),
    globals: [vm_mod.globals_size]Object,

    pub fn init(allocator: std.mem.Allocator) VmState {
        return .{
            .allocator = allocator,
            .vm_arena = std.heap.ArenaAllocator.init(allocator),
            .symbol_table = SymbolTable.init(allocator, null),
            .constants = .empty,
            .globals = undefined,
        };
    }

    pub fn deinit(self: *VmState) void {
        self.constants.deinit(self.allocator);
        self.symbol_table.deinit();
        self.vm_arena.deinit();
    }

    pub fn newCompiler(self: *VmState) !Compiler {
        var comp = Compiler.init(self.allocator, &self.symbol_table, &self.constants);
        try comp.enterScope();
        return comp;
    }

    pub fn newVm(self: *VmState, bytecode: Bytecode, writer: *std.Io.Writer) !Vm {
        return Vm.init(bytecode, &self.globals, self.vm_arena.allocator(), writer);
    }
};
