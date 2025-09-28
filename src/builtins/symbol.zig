//! Symbol operations for the Scheme interpreter.
//!
//! This module provides native implementations of symbol predicates
//! and conversion functions, including symbol?, symbol=?, symbol->string,
//! and string->symbol operations.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const NativeProc = @import("../NativeProc.zig");
const Proc = @import("../Proc.zig");
const String = @import("../types/String.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Registers all symbol functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register symbol functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&symbol_predicate_func);
    try vm.builder().defineNativeProc(&symbol_equal_func);
    try vm.builder().defineNativeProc(&symbol_to_string_func);
    try vm.builder().defineNativeProc(&string_to_symbol_func);
}

/// Native implementation of the `symbol?` predicate.
///
/// Tests whether the argument is a symbol. Returns #t if the argument
/// is a symbol, #f otherwise.
///
/// Args:
///   obj: The value to test for symbol type
///
/// Returns:
///   #t if the argument is a symbol, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const symbol_predicate_func = NativeProc.Native{
    .name = "symbol?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }
            return Val.init(args[0].isSymbol());
        }
    }.func,
};

/// Native implementation of the `symbol=?` comparison predicate.
///
/// Tests whether all arguments are symbols and are equal to each other.
/// Returns #t if all arguments are symbols and equal, #f otherwise.
///
/// Args:
///   symbol1, symbol2, symbol3, ...: Two or more symbols to compare
///
/// Returns:
///   #t if all arguments are symbols and equal, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a symbol
const symbol_equal_func = NativeProc.Native{
    .name = "symbol=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            // Check that all arguments are symbols
            for (args) |arg| {
                switch (arg.repr) {
                    .symbol => {},
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                }
            }

            // Compare all symbols to the first one
            const first_symbol = args[0].repr.symbol;
            for (args[1..]) |arg| {
                const current_symbol = arg.repr.symbol;
                if (!first_symbol.eql(current_symbol)) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `symbol->string` conversion function.
///
/// Converts a symbol to its string representation. The resulting string
/// contains the same character sequence as the symbol's name.
///
/// Args:
///   symbol: The symbol to convert to a string
///
/// Returns:
///   A string containing the symbol's name
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a symbol
const symbol_to_string_func = NativeProc.Native{
    .name = "symbol->string",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const symbol_interned = switch (args[0].repr) {
                .symbol => |s| s,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            // Get the symbol from the interner
            const symbol = ctx.vm.interner.get(symbol_interned) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };

            // Create a string from the symbol's data
            const string = String.init(ctx.vm.allocator, symbol.data) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };

            return ctx.vm.toVal(string) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
        }
    }.func,
};

/// Native implementation of the `string->symbol` conversion function.
///
/// Converts a string to a symbol. The resulting symbol will have the same
/// name as the string's contents. Symbols created from identical strings
/// will be the same symbol (eq?).
///
/// Args:
///   string: The string to convert to a symbol
///
/// Returns:
///   A symbol with the same name as the string
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a string
const string_to_symbol_func = NativeProc.Native{
    .name = "string->symbol",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const string_handle = switch (args[0].repr) {
                .string => |h| h,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            // Resolve the string handle
            const string = ctx.vm.inspector().resolve(String, string_handle) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };

            // Create a symbol from the string's slice
            const symbol = Symbol.init(string.slice());

            // Intern the symbol
            const interned_symbol = ctx.vm.interner.intern(symbol) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };

            return Val.init(interned_symbol);
        }
    }.func,
};

test "symbol? returns #t for symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(symbol? 'hello)");
    try vm.expectEval("#t", "(symbol? 'world)");
    try vm.expectEval("#t", "(symbol? 'test-symbol)");
}

test "symbol? returns #f for non-symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(symbol? 42)");
    try vm.expectEval("#f", "(symbol? #t)");
    try vm.expectEval("#f", "(symbol? \"hello\")");
    try vm.expectEval("#f", "(symbol? '())");
    try vm.expectEval("#f", "(symbol? '(a b c))");
}

test "symbol? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol? 'a 'b)"),
    );
}

test "symbol=? returns #t for equal symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(symbol=? 'hello 'hello)");
    try vm.expectEval("#t", "(symbol=? 'test 'test 'test)");
    try vm.expectEval("#t", "(symbol=? 'a 'a 'a 'a)");
}

test "symbol=? returns #f for different symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(symbol=? 'hello 'world)");
    try vm.expectEval("#f", "(symbol=? 'test 'test 'different)");
    try vm.expectEval("#f", "(symbol=? 'a 'b)");
}

test "symbol=? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol=?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol=? 'single)"),
    );
}

test "symbol=? with non-symbol arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol=? 'hello 42)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol=? 'hello \"world\")"),
    );
}

test "symbol->string converts symbols to strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"hello\"", "(symbol->string 'hello)");
    try vm.expectEval("\"world\"", "(symbol->string 'world)");
    try vm.expectEval("\"test-symbol\"", "(symbol->string 'test-symbol)");
}

test "symbol->string with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol->string)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol->string 'a 'b)"),
    );
}

test "symbol->string with non-symbol argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol->string 42)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(symbol->string \"hello\")"),
    );
}

test "string->symbol converts strings to symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("hello", "(string->symbol \"hello\")");
    try vm.expectEval("world", "(string->symbol \"world\")");
    try vm.expectEval("test-symbol", "(string->symbol \"test-symbol\")");
}

test "string->symbol with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(string->symbol)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(string->symbol \"a\" \"b\")"),
    );
}

test "string->symbol with non-string argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(string->symbol 42)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(string->symbol 'hello)"),
    );
}

test "round-trip conversion preserves symbols" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(symbol=? 'hello (string->symbol (symbol->string 'hello)))");
    try vm.expectEval("#t", "(symbol=? 'test-123 (string->symbol (symbol->string 'test-123)))");
}

test "round-trip conversion preserves strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? \"hello\" (symbol->string (string->symbol \"hello\")))");
    try vm.expectEval("#t", "(equal? \"test-456\" (symbol->string (string->symbol \"test-456\")))");
}

test "symbols from identical strings are equal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(symbol=? (string->symbol \"hello\") (string->symbol \"hello\"))");
    try vm.expectEval("#t", "(eq? (string->symbol \"world\") (string->symbol \"world\"))");
}

test "symbol conversion with empty string" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"\"", "(symbol->string (string->symbol \"\"))");
    try vm.expectEval("#t", "(symbol=? (string->symbol \"\") (string->symbol \"\"))");
}
