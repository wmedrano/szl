const std = @import("std");
const testing = std.testing;

const PrettyPrinter = @import("../utils/PrettyPrinter.zig");
const Vm = @import("../Vm.zig");
const Box = @import("Box.zig");
const Continuation = @import("Continuation.zig");
const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const NativeProc = @import("NativeProc.zig");
pub const Number = @import("number.zig").Number;
const Pair = @import("Pair.zig");
const Parameter = @import("Parameter.zig");
const Proc = @import("Proc.zig");
pub const Rational = @import("number.zig").Rational;
const Record = @import("Record.zig");
const String = @import("String.zig");
const Symbol = @import("Symbol.zig");
const SyntaxRules = @import("SyntaxRules.zig");

pub const Vector = std.ArrayList(Val);
pub const ByteVector = std.ArrayList(u8);

const Val = @This();

data: Data,

pub const Data = union(enum) {
    boolean: bool,
    empty_list: void,
    int: i64,
    rational: Rational,
    float: f64,
    char: u21,
    pair: Handle(Pair),
    string: Handle(String),
    symbol: Symbol,
    module: Handle(Module),
    proc: Handle(Proc),
    native_proc: *const NativeProc,
    vector: Handle(Vector),
    bytevector: Handle(ByteVector),
    box: Handle(Box),
    continuation: Handle(Continuation),
    syntax_rules: Handle(SyntaxRules),
    record: Handle(Record),
    record_descriptor: Handle(Record.Descriptor),
    parameter: Handle(Parameter),
};

pub fn initEmptyList() Val {
    return Val{ .data = .{ .empty_list = {} } };
}

pub fn initInt(x: i64) Val {
    return Val{ .data = .{ .int = x } };
}

pub fn initBool(x: bool) Val {
    return Val{ .data = .{ .boolean = x } };
}

pub fn initRational(rational: Rational) Val {
    // If the rational reduces to a whole number, return an integer instead
    if (rational.denominator == 1) {
        return Val.initInt(rational.numerator);
    }
    return Val{ .data = .{ .rational = rational } };
}

pub fn initFloat(x: f64) Val {
    return Val{ .data = .{ .float = x } };
}

pub fn initChar(c: u21) Val {
    return Val{ .data = .{ .char = c } };
}

pub fn initSymbol(sym: Symbol) Val {
    return Val{ .data = .{ .symbol = sym } };
}

pub fn initModule(mod: Handle(Module)) Val {
    return Val{ .data = .{ .module = mod } };
}

pub fn initProc(proc: Handle(Proc)) Val {
    return Val{ .data = .{ .proc = proc } };
}

pub fn initClosure(closure: Handle(Proc)) Val {
    return Val{ .data = .{ .proc = closure } };
}

pub fn initNativeProc(proc: *const NativeProc) Val {
    return Val{ .data = .{ .native_proc = proc } };
}

pub fn initContinuation(continuation: Handle(Continuation)) Val {
    return Val{ .data = .{ .continuation = continuation } };
}

pub fn initParameter(parameter: Handle(Parameter)) Val {
    return Val{ .data = .{ .parameter = parameter } };
}

pub fn isNull(self: Val) bool {
    return self.data == .empty_list;
}

pub fn isTruthy(self: Val) bool {
    switch (self.data) {
        .boolean => |x| return x,
        else => return true,
    }
}

pub fn isProc(self: Val) bool {
    return switch (self.data) {
        .continuation,
        .native_proc,
        .parameter,
        .proc,
        => true,
        else => false,
    };
}

pub fn asSymbol(self: Val) ?Symbol {
    switch (self.data) {
        .symbol => |s| return s,
        else => return null,
    }
}

pub fn asBool(self: Val) ?bool {
    switch (self.data) {
        .boolean => |x| return x,
        else => return null,
    }
}

pub fn asInt(self: Val) ?i64 {
    if (self.data == .int) return self.data.int;
    return null;
}

pub fn asFloat(self: Val) ?f64 {
    if (self.data == .float) return self.data.float;
    return null;
}

pub fn asChar(self: Val) ?u21 {
    if (self.data == .char) return self.data.char;
    return null;
}

pub fn asRational(self: Val) ?Rational {
    if (self.data == .rational) return self.data.rational;
    return null;
}

pub fn asNumber(self: Val) ?Number {
    return switch (self.data) {
        .int => |x| Number{ .int = x },
        .rational => |x| Number{ .rational = x },
        .float => |x| Number{ .float = x },
        else => null,
    };
}

pub fn asParameter(self: Val) ?Handle(Parameter) {
    if (self.data == .parameter) return self.data.parameter;
    return null;
}

pub fn eq(self: Val, other: Val) bool {
    return std.meta.eql(self, other);
}

test "Val is small" {
    try testing.expectEqual(16, @sizeOf(Val));
}
