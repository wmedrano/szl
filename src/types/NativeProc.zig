const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Vm = @import("../Vm.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

/// NativeProc represents a Scheme procedure implemented in Zig (native code).
const NativeProc = @This();

name: []const u8,
docstring: []const u8,
unsafe_impl: *const fn (*Vm, ?*Diagnostics, arg_count: u32) Vm.Error!void,

/// Create a NativeProc for variadic functions (variable number of arguments).
///
/// The implementation function receives all arguments as a slice `[]Val`.
/// This is appropriate for functions like `+`, `list`, etc. that accept any number of args.
///
/// # Example:
/// ```zig
/// pub const my_variadic = NativeProc.withRawArgs(struct {
///     pub const name = "my-variadic";
///     pub const docstring = "(my-variadic . args) - Takes any number of args";
///     pub fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []Val) Vm.Error!Val {
///         // Process all arguments
///         var sum: i64 = 0;
///         for (args) |arg| {
///             const num = arg.asInt() orelse {
///                 if (diagnostics) |d| {
///                     d.addDiagnostic(.{ .wrong_arg_type = .{
///                         .expected = "int",
///                         .proc = Val.initNativeProc(&def),
///                         .got = arg,
///                     } });
///                 }
///                 return Vm.Error.UncaughtException;
///             };
///             sum += num;
///         }
///         return Val.initInt(sum);
///     }
/// });
/// ```
pub fn withRawArgs(T: type) NativeProc {
    return struct {
        const def = NativeProc{
            .name = T.name,
            .unsafe_impl = &wrapped,
            .docstring = if (@hasDecl(T, "docstring")) T.docstring else "",
        };

        fn wrapped(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
            const args = vm.context.stackTopN(arg_count);
            const val = try T.impl(vm, diagnostics, args);
            _ = vm.context.popStackFrame(.discard);
            try vm.context.push(vm.allocator(), val);
        }
    }.def;
}

/// Create a NativeProc for functions taking exactly 1 argument.
///
/// Automatically validates argument count and reports errors via diagnostics.
/// The implementation function receives the single argument directly as `Val`.
///
/// # Example:
/// ```zig
/// pub const not = NativeProc.with1Arg(struct {
///     pub const name = "not";
///     pub const docstring = "(not obj) - Returns #t if obj is #f, otherwise #f";
///     pub fn impl(_: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
///         return Val.initBool(!arg.isTruthy());
///     }
/// });
/// ```
pub fn with1Arg(T: type) NativeProc {
    return struct {
        const def = NativeProc{
            .name = T.name,
            .unsafe_impl = &wrapped,
            .docstring = if (@hasDecl(T, "docstring")) T.docstring else "",
        };

        fn wrapped(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
            if (arg_count != 1) {
                @branchHint(.cold);
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_count = .{
                        .expected = 1,
                        .got = arg_count,
                        .proc = Val.initNativeProc(&def),
                    } });
                }
                return Vm.Error.UncaughtException;
            }
            const val = try T.impl(vm, diagnostics, vm.context.top().?);
            _ = vm.context.popStackFrame(.discard);
            try vm.context.push(vm.allocator(), val);
        }
    }.def;
}

pub fn with2Args(T: type) NativeProc {
    return struct {
        const def = NativeProc{
            .name = T.name,
            .unsafe_impl = &wrapped,
            .docstring = if (@hasDecl(T, "docstring")) T.docstring else "",
        };

        fn wrapped(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
            if (arg_count != 2) {
                @branchHint(.cold);
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_count = .{
                        .expected = 2,
                        .got = arg_count,
                        .proc = Val.initNativeProc(&def),
                    } });
                }
                return Vm.Error.UncaughtException;
            }
            const args = vm.context.stackTopN(2);
            const val = try T.impl(vm, diagnostics, args[0], args[1]);
            _ = vm.context.popStackFrame(.discard);
            try vm.context.push(vm.allocator(), val);
        }
    }.def;
}
