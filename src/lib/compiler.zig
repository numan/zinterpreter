const std = @import("std");

const code = @import("code.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");
const symbol_table = @import("symbol_table.zig");

const Object = object.Object;
const Program = ast.Program;
const StatementType = ast.StatementType;
const ExpressionType = ast.ExpressionType;

const SymbolTable = symbol_table.SymbolTable;

const Error = error{
    UnsupportedNodeType,
    OperationNotSupported,
    UndefinedVariable,
} || std.mem.Allocator.Error;

const EmittedInstruction = struct {
    opcode: code.Opcode,
    position: usize,
};

const CompilationScope = struct {
    instructions: std.ArrayList(u8) = .empty,
    last_instruction: ?EmittedInstruction = null,
    previous_instruction: ?EmittedInstruction = null,
};

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,
    constants: *std.ArrayList(Object),
    constants_allocator: std.mem.Allocator,
    symbol_table: *SymbolTable,
    scopes: std.ArrayList(CompilationScope),
    scope_index: usize = 0,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator, st: *SymbolTable, constants: *std.ArrayList(Object), constants_alloc: std.mem.Allocator) Compiler {
        return Compiler{
            .arena = std.heap.ArenaAllocator.init(alloc),
            .constants = constants,
            .constants_allocator = constants_alloc,
            .symbol_table = st,
            .scopes = .empty,
        };
    }

    pub fn enterScope(self: *Self) !void {
        try self.scopes.append(self.allocator(), .{});
        self.scope_index = self.scopes.items.len - 1;
        // Only create an enclosed symbol table for nested scopes (functions),
        // not for the initial main program scope
        if (self.scopes.items.len > 1) {
            const enclosed = try self.allocator().create(SymbolTable);
            enclosed.* = SymbolTable.initEnclosed(self.allocator(), self.symbol_table);
            self.symbol_table = enclosed;
        }
    }

    pub fn leaveScope(self: *Self) struct { instructions: ?code.Instructions, num_locals: usize } {
        if (self.scope_index == 0) {
            return .{ .instructions = null, .num_locals = 0 };
        }

        const num_locals = self.symbol_table.num_definitions;
        if (self.symbol_table.outer) |outer| {
            self.symbol_table = outer;
        }

        self.scope_index -= 1;
        const scope = self.scopes.pop() orelse return .{ .instructions = null, .num_locals = 0 };
        return .{ .instructions = scope.instructions.items, .num_locals = num_locals };
    }

    fn currentScope(self: *Self) *CompilationScope {
        return &self.scopes.items[self.scope_index];
    }

    fn setLastInstruction(self: *Self, opcode: code.Opcode, position: usize) void {
        const scope = self.currentScope();
        scope.previous_instruction = scope.last_instruction;
        scope.last_instruction = .{ .opcode = opcode, .position = position };
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn deinit(self: *Self) void {
        for (self.scopes.items) |*s| {
            s.instructions.deinit(self.allocator());
        }
        self.scopes.deinit(self.allocator());
        self.arena.deinit();
    }

    pub fn compile(self: *Self, node: anytype) Error!void {
        return switch (@TypeOf(node)) {
            *Program, *const Program => self.compileProgram(node),
            *StatementType, *const StatementType => self.compileStatement(node),
            *StatementType.ExpressionStatement, *const StatementType.ExpressionStatement => self.compile(&node.expression),
            *ExpressionType, *const ExpressionType => try self.compileExpression(node),
            *StatementType.BlockStatement, *const StatementType.BlockStatement => self.compileBlockStatement(node),
            inline else => @compileError("unsupported node type for compile"),
        };
    }

    fn compileProgram(self: *Self, program: *const Program) !void {
        for (program.statements.items) |*stmt| {
            try self.compile(stmt);
        }
    }

    fn compileBlockStatement(self: *Self, block_statement: *const StatementType.BlockStatement) !void {
        for (block_statement.statements.items) |*stmt| {
            try self.compile(stmt);
        }
    }

    fn compileStatement(self: *Self, statement: *const StatementType) !void {
        return switch (statement.*) {
            .expression => |*expression_statement| {
                try self.compile(expression_statement);
                _ = try self.emit(.pop, &.{});
            },
            .let => |*let_stmt| {
                try self.compile(&let_stmt.value.expression);
                const sym = try self.symbol_table.define(let_stmt.name.value);
                const op: code.Opcode = if (sym.scope == .global) .set_global else .set_local;
                _ = try self.emit(op, &.{sym.index});
            },
            .@"return" => |*ret_stmt| {
                if (ret_stmt.value) |*val| {
                    try self.compile(&val.expression);
                }
                _ = try self.emit(.return_value, &.{});
            },
            else => Error.UnsupportedNodeType,
        };
    }

    fn compileExpression(self: *Self, expression: *const ExpressionType) !void {
        return switch (expression.*) {
            .string_literal => |*val| self.compileStringLiteral(val),
            .array_literal => |*val| self.compileArrayLiteral(val),
            .hash_literal => |*val| self.compileHashLiteral(val),
            .integer_literal => |*val| self.compileIntegerLiteral(val),
            .boolean_literal => |*val| self.compileBooleanLiteral(val),
            .infix_expression => |*val| self.compileInfixExpression(val),
            .prefix_expression => |*val| self.compilePrefixExpression(val),
            .if_expression => |*val| self.compileIfExpression(val),
            .identifier => |*ident| {
                const sym = self.symbol_table.resolve(ident.value) orelse return Error.UndefinedVariable;
                const op: code.Opcode = switch (sym.scope) {
                    .global => .get_global,
                    .local => .get_local,
                    .builtin => .get_builtin,
                };
                _ = try self.emit(op, &.{sym.index});
            },
            .index_expression => |*idx| {
                try self.compile(&idx.left.expression);
                try self.compile(&idx.index.expression);
                _ = try self.emit(.index, &.{});
            },
            .function_literal => |*fn_lit| {
                try self.compileFunctionLiteral(fn_lit);
            },
            .call_expression => |*val| self.compileCallExpression(val),
            else => Error.UnsupportedNodeType,
        };
    }

    fn compileCallExpression(self: *Self, call_expression: *const ExpressionType.CallExpression) !void {
        try self.compile(call_expression.function);
        for (call_expression.arguments) |*arg| {
            try self.compile(arg);
        }
        _ = try self.emit(.call, &.{call_expression.arguments.len});
    }

    fn compileArrayLiteral(self: *Self, array_literal: *const ExpressionType.ArrayLiteral) !void {
        const size = array_literal.elements.len;

        for (array_literal.elements) |*i| {
            try self.compile(i);
        }

        _ = try self.emit(.array, &.{size});
    }

    fn compileHashLiteral(self: *Self, hash_literal: *const ExpressionType.HashLiteral) !void {
        for (hash_literal.pairs) |*pair| {
            try self.compile(&pair.key.expression);
            try self.compile(&pair.value.expression);
        }
        _ = try self.emit(.hash, &.{hash_literal.pairs.len * 2});
    }

    fn compileStringLiteral(self: *Self, string_literal: *const ExpressionType.StringLiteral) !void {
        const heap_string_obj = try self.allocator().create(Object.String);
        heap_string_obj.* = Object.String.init(string_literal.value);
        const str_obj: Object = .{ .string = heap_string_obj };
        const index = try self.addConstant(str_obj);
        _ = try self.emit(.constant, &.{index});
    }

    fn compileIfExpression(self: *Self, if_expression: *const ExpressionType.IfExpression) !void {
        try self.compile(if_expression.condition);

        const jump_not_truthy_pos = try self.emit(.jump_not_truthy, &.{9999});

        try self.compile(&if_expression.consequence);

        self.removeLast(.pop);

        // Always emit OpJump after consequence
        const jump_pos = try self.emit(.jump, &.{9999});
        const after_consequence_pos = self.currentScope().instructions.items.len;
        try self.changeOperand(jump_not_truthy_pos, after_consequence_pos);

        if (if_expression.alternative) |*alt| {
            try self.compile(alt);
            self.removeLast(.pop);
        } else {
            _ = try self.emit(.op_null, &.{});
        }

        const after_alternative_pos = self.currentScope().instructions.items.len;
        try self.changeOperand(jump_pos, after_alternative_pos);
    }

    fn compileIntegerLiteral(self: *Self, int_literal: *const ExpressionType.IntegerLiteral) !void {
        const int_obj: Object = .{ .int = Object.Integer.init(int_literal.value) };
        const index = try self.addConstant(int_obj);
        _ = try self.emit(.constant, &.{index});
    }

    fn compileBooleanLiteral(self: *Self, bool_literal: *const ExpressionType.BooleanLiteral) !void {
        if (bool_literal.value) {
            _ = try self.emit(.op_true, &.{});
        } else {
            _ = try self.emit(.op_false, &.{});
        }
    }

    fn compilePrefixExpression(self: *Self, prefix_expression: *const ExpressionType.PrefixExpression) !void {
        try self.compile(&prefix_expression.right.expression);

        switch (prefix_expression.token.token_type) {
            .minus => _ = try self.emit(.minus, &.{}),
            .bang => _ = try self.emit(.bang, &.{}),
            else => return Error.OperationNotSupported,
        }
    }

    fn compileInfixExpression(self: *Self, infix_expression: *const ExpressionType.InfixExpression) !void {
        // For less-than, swap operands and use greater-than
        if (infix_expression.token.token_type == .lt) {
            try self.compile(infix_expression.right);
            try self.compile(infix_expression.left);
            _ = try self.emit(.greater_than, &.{});
            return;
        }

        try self.compile(infix_expression.left);
        try self.compile(infix_expression.right);

        switch (infix_expression.token.token_type) {
            .plus => _ = try self.emit(.add, &.{}),
            .minus => _ = try self.emit(.sub, &.{}),
            .asterisk => _ = try self.emit(.mul, &.{}),
            .slash => _ = try self.emit(.div, &.{}),
            .gt => _ = try self.emit(.greater_than, &.{}),
            .eq => _ = try self.emit(.equal, &.{}),
            .not_eq => _ = try self.emit(.not_equal, &.{}),
            else => return Error.OperationNotSupported,
        }
    }

    fn compileFunctionLiteral(self: *Self, fn_lit: *const ExpressionType.FunctionLiteral) !void {
        try self.enterScope();

        for (fn_lit.*.parameters) |*param| {
            _ = try self.symbol_table.define(param.*.value);
        }

        try self.compile(&fn_lit.body);

        try self.replaceLastPopWithReturn();

        if (!self.lastInstructionIs(.return_value)) {
            _ = try self.emit(.op_return, &.{});
        }

        const result = self.leaveScope();
        const instructions = result.instructions orelse return Error.OperationNotSupported;

        const compiled_fn = try self.allocator().create(Object.CompiledFunction);
        compiled_fn.* = Object.CompiledFunction.init(instructions, result.num_locals, fn_lit.parameters.len);
        _ = try self.emit(.constant, &.{
            try self.addConstant(.{
                .compiled_function = compiled_fn,
            }),
        });
    }

    pub fn bytecode(self: *Self) Bytecode {
        return .{
            .instructions = self.currentScope().instructions.items,
            .constants = self.constants.items,
        };
    }

    fn emit(self: *Self, op: code.Opcode, operands: []const usize) !usize {
        const alloc = self.allocator();
        const inst = try code.make(alloc, op, operands);
        const pos = try self.addInstruction(inst);

        self.setLastInstruction(op, pos);

        return pos;
    }

    fn addInstruction(self: *Self, ins: code.Instructions) !usize {
        const alloc = self.allocator();
        const scope = self.currentScope();
        const new_instruction_position = scope.instructions.items.len;
        try scope.instructions.appendSlice(alloc, ins);
        return new_instruction_position;
    }

    fn addConstant(self: *Self, obj: Object) !usize {
        try self.constants.append(self.constants_allocator, obj);
        return self.constants.items.len - 1;
    }

    fn replaceInstruction(self: *Self, pos: usize, new_instruction: code.Instructions) void {
        const scope = self.currentScope();
        var offset = pos;
        for (new_instruction) |byte| {
            scope.instructions.items[offset] = byte;
            offset += 1;
        }
    }

    fn changeOperand(self: *Self, op_pos: usize, operand: usize) !void {
        const op: code.Opcode = @enumFromInt(self.currentScope().instructions.items[op_pos]);
        const new_instruction = try code.make(self.allocator(), op, &.{operand});

        self.replaceInstruction(op_pos, new_instruction);
    }

    fn replaceLastPopWithReturn(self: *Self) !void {
        if (!self.lastInstructionIs(.pop)) {
            return;
        }

        const current_scope = self.currentScope();

        if (current_scope.last_instruction == null) {
            return Error.OperationNotSupported;
        }

        const last_pos = self.currentScope().last_instruction.?.position;
        const return_instruction = try code.make(self.allocator(), .return_value, &.{});
        self.replaceInstruction(last_pos, return_instruction);
        current_scope.last_instruction.?.opcode = .return_value;
    }

    fn lastInstructionIs(self: *Self, op: code.Opcode) bool {
        if (self.currentScope().last_instruction) |last| {
            return last.opcode == op;
        }
        return false;
    }

    fn removeLast(self: *Self, op: code.Opcode) void {
        if (self.lastInstructionIs(op)) {
            const scope = self.currentScope();
            scope.instructions.items = scope.instructions.items[0..scope.last_instruction.?.position];
            scope.last_instruction = scope.previous_instruction;
        }
    }
};

pub const Bytecode = struct {
    instructions: code.Instructions,
    constants: []Object,
};
