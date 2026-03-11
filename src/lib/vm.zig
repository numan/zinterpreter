const std = @import("std");
const code = @import("code.zig");
const compiler = @import("compiler.zig");
const object = @import("object.zig");

const Object = object.Object;
const Bytecode = compiler.Bytecode;

const True = Object{ .bool = Object.Boolean.init(true) };
const False = Object{ .bool = Object.Boolean.init(false) };
const Null = Object{ .null = Object.Null.init() };

const stack_size = 2048;
pub const globals_size = 65536;

const Errors = error{
    StackOverflow,
    UnknownOpcode,
    HashNotHashable,
};

pub const Vm = struct {
    constants: []Object,
    instructions: code.Instructions,
    stack: [stack_size]Object,
    sp: usize, // stack pointer — always points to next free slot
    globals: *[globals_size]Object,
    allocator: std.mem.Allocator,

    pub fn init(bytecode: Bytecode, globals: *[globals_size]Object, allocator: std.mem.Allocator) Vm {
        return .{
            .constants = bytecode.constants,
            .instructions = bytecode.instructions,
            .stack = undefined,
            .sp = 0,
            .globals = globals,
            .allocator = allocator,
        };
    }

    pub fn stackTop(self: *const Vm) ?Object {
        if (self.sp == 0) return null;
        return self.stack[self.sp - 1];
    }

    pub fn lastPoppedStackElem(self: *const Vm) ?Object {
        if (self.sp == stack_size) return null;
        return self.stack[self.sp];
    }

    pub fn run(self: *Vm) !void {
        var ip: usize = 0;
        while (ip < self.instructions.len) {
            const op: code.Opcode = @enumFromInt(self.instructions[ip]);
            switch (op) {
                .constant => {
                    const const_index = code.readUint16(self.instructions[ip + 1 ..]);
                    ip += 2;
                    try self.push(self.constants[const_index]);
                },
                .add, .sub, .mul, .div => {
                    const right = try self.pop();
                    const left = try self.pop();
                    try self.executeBinaryOp(op, left, right);
                },
                .op_true => {
                    try self.push(True);
                },
                .op_false => {
                    try self.push(False);
                },
                .equal, .not_equal, .greater_than => {
                    const right = try self.pop();
                    const left = try self.pop();
                    try self.executeComparison(op, left, right);
                },
                .jump => {
                    const pos = code.readUint16(self.instructions[ip + 1 ..]);
                    ip = pos - 1; // loop will increment
                },
                .jump_not_truthy => {
                    const pos = code.readUint16(self.instructions[ip + 1 ..]);
                    ip += 2; // skip operand
                    const condition = try self.pop();
                    if (!isTruthy(condition)) {
                        ip = pos - 1; // loop will increment
                    }
                },
                .array => {
                    const len = code.readUint16(self.instructions[ip + 1 ..]);
                    ip += 2; //skip operand
                    const array = try self.buildArray(self.sp - len, self.sp);
                    self.sp -= len; // remove array elements from stack
                    try self.push(.{ .array = array });
                },
                .hash => {
                    const num_elements = code.readUint16(self.instructions[ip + 1 ..]);
                    ip += 2;
                    const hash = try self.buildHash(self.sp - num_elements, self.sp);
                    self.sp -= num_elements;
                    try self.push(.{ .hash = hash });
                },
                .op_null => {
                    try self.push(Null);
                },
                .minus => {
                    const operand = try self.pop();
                    const int_val = switch (operand) {
                        .int => |v| v.value,
                        else => return Errors.UnknownOpcode,
                    };
                    try self.push(.{ .int = Object.Integer.init(-int_val) });
                },
                .bang => {
                    const operand = try self.pop();
                    const result = switch (operand) {
                        .bool => |b| if (b.value) False else True,
                        .null => True,
                        else => False,
                    };
                    try self.push(result);
                },
                .set_global => {
                    const global_index = code.readUint16(self.instructions[ip + 1 ..]);
                    ip += 2;
                    self.globals[global_index] = try self.pop();
                },
                .get_global => {
                    const global_index = code.readUint16(self.instructions[ip + 1 ..]);
                    ip += 2;
                    try self.push(self.globals[global_index]);
                },
                .pop => {
                    _ = try self.pop();
                },
            }
            ip += 1;
        }
    }

    fn executeComparison(self: *Vm, op: code.Opcode, left: Object, right: Object) !void {
        if (left == .int and right == .int) {
            return self.executeIntegerComparison(op, left.int.value, right.int.value);
        }

        const result = switch (op) {
            .equal => std.meta.eql(left, right),
            .not_equal => !std.meta.eql(left, right),
            else => return Errors.UnknownOpcode,
        };

        try self.push(nativeBoolToBooleanObject(result));
    }

    fn executeIntegerComparison(self: *Vm, op: code.Opcode, left: i64, right: i64) !void {
        const result = switch (op) {
            .equal => left == right,
            .not_equal => left != right,
            .greater_than => left > right,
            else => return Errors.UnknownOpcode,
        };

        try self.push(nativeBoolToBooleanObject(result));
    }

    fn executeBinaryOp(self: *Vm, op: code.Opcode, left: Object, right: Object) !void {
        if (left == .int and right == .int) {
            return self.executeBinaryIntegerOp(op, left.int.value, right.int.value);
        }
        if (op == .add and left == .string and right == .string) {
            return self.executeStringConcatenation(left.string.value, right.string.value);
        }
        return Errors.UnknownOpcode;
    }

    fn executeBinaryIntegerOp(self: *Vm, op: code.Opcode, left: i64, right: i64) !void {
        const result = switch (op) {
            .add => left + right,
            .sub => left - right,
            .mul => left * right,
            .div => @divTrunc(left, right),
            else => unreachable,
        };
        try self.push(.{ .int = Object.Integer.init(result) });
    }

    fn executeStringConcatenation(self: *Vm, left: []const u8, right: []const u8) !void {
        const concat = try std.mem.concat(self.allocator, u8, &.{ left, right });
        const str = try self.allocator.create(Object.String);
        str.* = Object.String.init(concat);
        try self.push(.{ .string = str });
    }

    fn buildArray(self: *Vm, start_index: usize, end_index: usize) !*Object.Array {
        const elements = try self.allocator.dupe(Object, self.stack[start_index..end_index]);
        const array = try self.allocator.create(Object.Array);
        array.* = Object.Array.init(elements);
        return array;
    }

    fn buildHash(self: *Vm, start_index: usize, end_index: usize) !*Object.Hash {
        var pairs = std.AutoHashMap(Object.HashKey, Object.HashPair).init(self.allocator);
        var i = start_index;
        while (i < end_index) : (i += 2) {
            const key = self.stack[i];
            const value = self.stack[i + 1];
            const hash_key = key.hashKey() orelse return Errors.HashNotHashable;
            try pairs.put(hash_key, .{ .key = key, .value = value });
        }
        const hash = try self.allocator.create(Object.Hash);
        hash.* = .{ .pairs = pairs };
        return hash;
    }

    fn nativeBoolToBooleanObject(value: bool) Object {
        return if (value) True else False;
    }

    fn isTruthy(obj: Object) bool {
        return switch (obj) {
            .bool => |b| b.value,
            .null => false,
            else => true,
        };
    }

    fn pop(self: *Vm) !Object {
        if (self.sp == 0) return Errors.StackOverflow; // Underflow error
        const obj = self.stack[self.sp - 1];
        self.sp -= 1;
        return obj;
    }

    fn push(self: *Vm, obj: Object) !void {
        if (self.sp >= stack_size) return Errors.StackOverflow;
        self.stack[self.sp] = obj;
        self.sp += 1;
    }
};
