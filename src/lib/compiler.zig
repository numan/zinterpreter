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

    pub fn init(opcode: code.Opcode, position: usize) EmittedInstruction {
        return .{
            .opcode = opcode,
            .position = position,
        };
    }
};

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,
    instructions: std.ArrayList(u8),
    constants: std.ArrayList(Object),
    symbol_table: SymbolTable,
    lastInstruction: ?EmittedInstruction = null,
    previousInstruction: ?EmittedInstruction = null,

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator) Compiler {
        return .{
            .arena = std.heap.ArenaAllocator.init(alloc),
            .instructions = .empty,
            .constants = .empty,
            .symbol_table = SymbolTable.init(),
        };
    }

    fn setLastInstruction(self: *Self, opcode: code.Opcode, position: usize) void {
        self.previousInstruction = self.lastInstruction;
        self.lastInstruction = EmittedInstruction.init(opcode, position);
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn deinit(self: *Self) void {
        self.instructions.deinit(self.allocator());
        self.constants.deinit(self.allocator());
        self.symbol_table.deinit(self.allocator());
        self.arena.deinit();
    }

    pub fn compile(self: *Self, node: anytype) Error!void {
        return switch (@TypeOf(node)) {
            *Program, *const Program => self.evalProgram(node),
            *StatementType, *const StatementType => self.evalStatement(node),
            *StatementType.ExpressionStatement, *const StatementType.ExpressionStatement => self.evalExpressionStatement(node),
            *ExpressionType, *const ExpressionType => try self.evalExpression(node),
            *StatementType.BlockStatement, *const StatementType.BlockStatement => self.evalBlockStatement(node),
            inline else => @compileError("unsupported node type for eval"),
        };
    }

    fn evalProgram(self: *Self, program: *const Program) !void {
        for (program.statements.items) |*stmt| {
            try self.compile(stmt);
        }
    }

    fn evalBlockStatement(self: *Self, block_statement: *const StatementType.BlockStatement) !void {
        for (block_statement.statements.items) |*stmt| {
            try self.compile(stmt);
        }
    }

    fn evalStatement(self: *Self, statement: *const StatementType) !void {
        return switch (statement.*) {
            .expression => |*expression_statement| {
                try self.compile(expression_statement);
                _ = try self.emit(.pop, &.{});
            },
            .let => |*let_stmt| {
                try self.compile(&let_stmt.value.expression);
                const sym = try self.symbol_table.define(self.allocator(), let_stmt.name.value);
                _ = try self.emit(.set_global, &.{sym.index});
            },
            else => Error.UnsupportedNodeType,
        };
    }

    fn evalExpressionStatement(self: *Self, expression_statement: *const StatementType.ExpressionStatement) !void {
        return try self.compile(&expression_statement.expression);
    }

    fn evalExpression(self: *Self, expression: *const ExpressionType) !void {
        return switch (expression.*) {
            .integer_literal => self.evalIntegerLiteral(&expression.integer_literal),
            .boolean_literal => self.evalBooleanLiteral(&expression.boolean_literal),
            .infix_expression => |*infix_expression| self.evalInfixExpression(infix_expression),
            .prefix_expression => |*prefix_expression| self.evalPrefixExpression(prefix_expression),
            .if_expression => |*if_expresson| self.evalIfExpression(if_expresson),
            .identifier => |*ident| {
                const sym = self.symbol_table.resolve(ident.value) orelse return Error.UndefinedVariable;
                _ = try self.emit(.get_global, &.{sym.index});
            },
            else => Error.UnsupportedNodeType,
        };
    }

    fn evalIfExpression(self: *Self, if_expression: *const ExpressionType.IfExpression) !void {
        try self.compile(if_expression.condition);

        const jumpNotTruthyPos = try self.emit(.jump_not_truthy, &.{9999});

        try self.compile(&if_expression.consequence);

        self.removeLastPop();

        // Always emit OpJump after consequence
        const jumpPos = try self.emit(.jump, &.{9999});
        const afterConsequencePos = self.instructions.items.len;
        try self.changeOperand(jumpNotTruthyPos, afterConsequencePos);

        if (if_expression.alternative) |*alt| {
            try self.compile(alt);
            self.removeLastPop();
        } else {
            _ = try self.emit(.op_null, &.{});
        }

        const afterAlternativePos = self.instructions.items.len;
        try self.changeOperand(jumpPos, afterAlternativePos);
    }

    fn evalIntegerLiteral(self: *Self, int_literal: *const ExpressionType.IntegerLiteral) !void {
        const int_obj: Object = .{ .int = Object.Integer.init(int_literal.value) };
        const index = try self.addConstant(int_obj);
        _ = try self.emit(.constant, &.{index});
    }

    fn evalBooleanLiteral(self: *Self, bool_literal: *const ExpressionType.BooleanLiteral) !void {
        if (bool_literal.value) {
            _ = try self.emit(.op_true, &.{});
        } else {
            _ = try self.emit(.op_false, &.{});
        }
    }

    fn evalPrefixExpression(self: *Self, prefix_expression: *const ExpressionType.PrefixExpression) !void {
        try self.compile(&prefix_expression.right.expression);

        switch (prefix_expression.token.token_type) {
            .minus => _ = try self.emit(.minus, &.{}),
            .bang => _ = try self.emit(.bang, &.{}),
            else => return Error.OperationNotSupported,
        }
    }

    fn evalInfixExpression(self: *Self, infix_expression: *const ExpressionType.InfixExpression) !void {
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

    pub fn bytecode(self: Self) Bytecode {
        return .{
            .instructions = self.instructions.items,
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
        const new_instuction_position = self.instructions.items.len;
        try self.instructions.appendSlice(alloc, ins);
        return new_instuction_position;
    }

    fn addConstant(self: *Self, obj: Object) !usize {
        const alloc = self.allocator();
        try self.constants.append(alloc, obj);
        return self.constants.items.len - 1;
    }

    fn replaceInstruction(self: *Self, pos: usize, new_instruction: code.Instructions) void {
        var offset = pos;
        for (new_instruction) |byte| {
            self.instructions.items[offset] = byte;
            offset += 1;
        }
    }

    fn changeOperand(self: *Self, op_pos: usize, operand: usize) !void {
        const op: code.Opcode = @enumFromInt(self.instructions.items[op_pos]);
        const new_instruction = try code.make(self.allocator(), op, &.{operand});

        self.replaceInstruction(op_pos, new_instruction);
    }

    fn lastInstructionIsPop(self: *Self) bool {
        if (self.lastInstruction) |last| {
            return last.opcode == .pop;
        }
        return false;
    }

    fn removeLastPop(self: *Self) void {
        if (self.lastInstructionIsPop()) {
            self.instructions.items = self.instructions.items[0..self.lastInstruction.?.position];
            self.lastInstruction = self.previousInstruction;
        }
    }
};

pub const Bytecode = struct {
    instructions: code.Instructions,
    constants: []Object,
};
