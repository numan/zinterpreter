const std = @import("std");

const code = @import("code.zig");
const ast = @import("ast.zig");
const object = @import("object.zig");

const Object = object.Object;
const Program = ast.Program;
const StatementType = ast.StatementType;
const ExpressionType = ast.ExpressionType;

pub const Compiler = struct {
    arena: std.heap.ArenaAllocator,
    instructions: std.ArrayList(u8),
    constants: std.ArrayList(Object),

    const Self = @This();

    pub fn init(alloc: std.mem.Allocator) Compiler {
        return .{
            .arena = std.heap.ArenaAllocator.init(alloc),
            .instructions = .empty,
            .constants = .empty,
        };
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn deinit(self: *Self) void {
        self.instructions.deinit(self.allocator());
        self.constants.deinit(self.allocator());
        self.arena.deinit();
    }

    pub fn compile(self: *Self, node: anytype) std.mem.Allocator.Error!void {
        return switch (@TypeOf(node)) {
            *Program, *const Program => self.evalProgram(node),
            *StatementType, *const StatementType => self.evalStatement(node),
            *StatementType.ExpressionStatement, *const StatementType.ExpressionStatement => self.evalExpressionStatement(node),
            *ExpressionType, *const ExpressionType => try self.evalExpression(node),
            inline else => @compileError("unsupported node type for eval"),
        };
    }

    fn evalProgram(self: *Self, program: *const Program) !void {
        for (program.statements.items) |*stmt| {
            try self.compile(stmt);
        }
    }

    fn evalStatement(self: *Self, statement: *const StatementType) !void {
        return switch (statement.*) {
            .expression => |*expression_statement| try self.compile(expression_statement),
            else => unreachable,
        };
    }

    fn evalExpressionStatement(self: *Self, expression_statement: *const StatementType.ExpressionStatement) !void {
        return try self.compile(&expression_statement.expression);
    }

    fn evalExpression(self: *Self, expression: *const ExpressionType) !void {
        return switch (expression.*) {
            .integer_literal => self.evalIntegerLiteral(&expression.integer_literal),
            .infix_expression => |*infix_expression| self.evalInfixExpression(infix_expression),
            else => unreachable,
        };
    }

    fn evalIntegerLiteral(self: *Self, int_literal: *const ExpressionType.IntegerLiteral) !void {
        const alloc = self.allocator();
        const int_obj = try alloc.create(Object);
        int_obj.* = .{ .int = Object.Integer.init(int_literal.value) };
        const index = try self.addConstant(int_obj);
        _ = try self.emit(.constant, &.{index});
    }

    fn emit(self: *Self, op: code.Opcode, operands: []const usize) !usize {
        const alloc = self.allocator();
        const inst = try code.make(alloc, op, operands);
        const pos = try self.addInstruction(inst);
        return pos;
    }

    fn addInstruction(self: *Self, ins: code.Instructions) !usize {
        const alloc = self.allocator();
        const new_instuction_position = self.instructions.items.len;
        try self.instructions.appendSlice(alloc, ins);
        return new_instuction_position;
    }

    fn addConstant(self: *Self, obj: *const Object) !usize {
        const alloc = self.allocator();
        try self.constants.append(alloc, obj.*);
        return self.constants.items.len - 1;
    }

    fn evalInfixExpression(self: *Self, infix_expression: *const ExpressionType.InfixExpression) !void {
        try self.compile(infix_expression.left);
        try self.compile(infix_expression.right);
    }

    pub fn bytecode(self: Self) Bytecode {
        return .{
            .instructions = self.instructions.items,
            .constants = self.constants.items,
        };
    }
};

pub const Bytecode = struct {
    instructions: code.Instructions,
    constants: []Object,
};
