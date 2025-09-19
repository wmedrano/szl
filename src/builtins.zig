//! Built-in functions and procedures for the Scheme interpreter.
//!
//! This module provides the registration of native built-in functions that
//! are available to Scheme programs at runtime. These functions form the
//! core standard library of operations available in the interpreter.

const std = @import("std");

const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

/// Registers all built-in functions with the virtual machine.
///
/// This function should be called during VM initialization to make all
/// standard Scheme built-in procedures available to executing programs.
///
/// Args:
///   vm: Pointer to the VM instance to register built-ins with.
///
/// Errors:
///   May return allocation errors if registering built-ins fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().define(
        Symbol.init("+"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("+")),
            .implementation = Procedure.initNative(addFunc),
        }),
    );
    try vm.builder().define(
        Symbol.init("szl-define"),
        try vm.builder().build(Procedure{
            .name = try vm.interner.internStatic(Symbol.init("szl-define")),
            .implementation = Procedure.initNative(szlDefineFunc),
        }),
    );
}

/// Native implementation of the '+' arithmetic operator.
/// Adds all numeric arguments together, returning 0 for no arguments.
/// Expects all arguments to be numeric values that can be converted to i64.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   A Val containing the sum of all numeric arguments as an i64.
///
/// Panics:
///   Currently panics if any argument cannot be converted to i64 (error handling not yet implemented).
fn addFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    var sum: i64 = 0;
    for (args) |arg| {
        const num = ctx.vm.fromVal(i64, arg) catch @panic("+ called with invalid type, errors not yet supported");
        sum += num;
    }
    return Val.init(sum);
}
/// Native implementation of the 'szl-define' procedure for defining global variables.
/// Defines a global variable with the given symbol name and value.
/// Expects exactly two arguments: a symbol and a value to associate with it.
///
/// Args:
///   ctx: The procedure execution context containing the VM and local stack arguments.
///
/// Returns:
///   Returns an unspecified value (currently no explicit return).
///
/// Panics:
///   Currently panics if called with wrong number of arguments, invalid symbol type,
///   or if definition fails (error handling not yet implemented).
fn szlDefineFunc(ctx: Procedure.Context) Val {
    const args = ctx.localStack();
    if (args.len != 2) @panic("szl-define called with wrong number of args, errors not yet supported");
    const symbol = ctx.vm.fromVal(Symbol.Interned, args[0]) catch @panic("szl-define expected symbol, errors not yet supported");
    const val = args[1];
    ctx.vm.builder().define(symbol, val) catch @panic("szl-define failed, errors not yet supported");
    return Val.init({});
}
