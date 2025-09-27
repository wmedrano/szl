//! Variable definition function for the Scheme interpreter.
//!
//! This module provides the native implementation of the `define` special form
//! for creating global variable bindings in the Scheme environment.
//!
//! The `define` function is used internally by the compiler to implement
//! variable definitions rather than being registered as a builtin procedure.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Proc = @import("../Proc.zig");
const NativeProc = @import("../NativeProc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Native implementation of the `define` procedure.
///
/// Defines a global variable in the VM's environment with the specified symbol and value.
/// This function is used internally by the compiler when processing `define` expressions,
/// rather than being registered as a standard builtin procedure.
///
/// Args:
///   symbol: A symbol representing the variable name to define
///   value: The value to bind to the symbol
///
/// Returns:
///   The `*unspecified*` value on success
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the first argument is not a symbol
///   - `invalid-argument`: When the definition operation fails
pub const define_fn = NativeProc.Native{
    .name = "define",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }
            const symbol = ctx.vm.fromVal(Symbol.Interned, args[0]) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const val = args[1];
            ctx.vm.builder().define(symbol, val) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            return Val.init(ctx.vm.common_symbols.@"*unspecified*");
        }
    }.func,
};
