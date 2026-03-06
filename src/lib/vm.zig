const std = @import("std");
const code = @import("code.zig");
const compiler = @import("compiler.zig");
const object = @import("object.zig");

const Object = object.Object;
const Bytecode = compiler.Bytecode;

const stack_size = 2048;

const Errors = error{
    StackOverflow,
    UnknownOpcode,
};

pub const Vm = struct {
    constants: []Object,
    instructions: code.Instructions,
    stack: [stack_size]Object,
    sp: usize, // stack pointer — always points to next free slot

    pub fn init(bytecode: Bytecode) Vm {
        return .{
            .constants = bytecode.constants,
            .instructions = bytecode.instructions,
            .stack = undefined,
            .sp = 0,
        };
    }

    pub fn stackTop(self: *const Vm) ?Object {
        if (self.sp == 0) return null;
        return self.stack[self.sp - 1];
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
                .add => {
                    const right = try self.pop();
                    const left = try self.pop();

                    const left_int = switch (left) { .int => |v| v.value, else => return Errors.UnknownOpcode };
                    const right_int = switch (right) { .int => |v| v.value, else => return Errors.UnknownOpcode };

                    try self.push(.{ .int = Object.Integer.init(left_int + right_int) });
                },
            }
            ip += 1;
        }
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
