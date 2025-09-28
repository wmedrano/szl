//! String operations for the Scheme interpreter.
//!
//! This module provides native implementations of string predicates,
//! comparison functions, case conversion, manipulation, and conversion
//! operations according to the R7RS Scheme specification.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const NativeProc = @import("../NativeProc.zig");
const Proc = @import("../Proc.zig");
const Char = @import("../types/Char.zig");
const Pair = @import("../types/Pair.zig");
const String = @import("../types/String.zig");
const Val = @import("../types/Val.zig");
const Vector = @import("../types/Vector.zig");
const Vm = @import("../Vm.zig");

/// Registers all string functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register string functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&string_predicate_func);
    try vm.builder().defineNativeProc(&make_string_func);
    try vm.builder().defineNativeProc(&string_func);
    try vm.builder().defineNativeProc(&string_length_func);
    try vm.builder().defineNativeProc(&string_ref_func);
    try vm.builder().defineNativeProc(&string_set_func);
    try vm.builder().defineNativeProc(&string_equal_func);
    try vm.builder().defineNativeProc(&string_ci_equal_func);
    try vm.builder().defineNativeProc(&string_less_func);
    try vm.builder().defineNativeProc(&string_ci_less_func);
    try vm.builder().defineNativeProc(&string_greater_func);
    try vm.builder().defineNativeProc(&string_ci_greater_func);
    try vm.builder().defineNativeProc(&string_less_equal_func);
    try vm.builder().defineNativeProc(&string_ci_less_equal_func);
    try vm.builder().defineNativeProc(&string_greater_equal_func);
    try vm.builder().defineNativeProc(&string_ci_greater_equal_func);
    try vm.builder().defineNativeProc(&string_upcase_func);
    try vm.builder().defineNativeProc(&string_downcase_func);
    try vm.builder().defineNativeProc(&string_foldcase_func);
    try vm.builder().defineNativeProc(&substring_func);
    try vm.builder().defineNativeProc(&string_append_func);
    try vm.builder().defineNativeProc(&string_to_list_func);
    try vm.builder().defineNativeProc(&list_to_string_func);
    try vm.builder().defineNativeProc(&string_copy_func);
    try vm.builder().defineNativeProc(&string_copy_bang_func);
    try vm.builder().defineNativeProc(&string_fill_bang_func);
}

/// Helper function to convert character to lowercase
fn charToLower(ch: u8) u8 {
    const delta = 'a' - 'A';
    return if (ch >= 'A' and ch <= 'Z') ch + delta else ch;
}

/// Helper function to convert character to uppercase
fn charToUpper(ch: u8) u8 {
    const delta = 'a' - 'A';
    return if (ch >= 'a' and ch <= 'z') ch - delta else ch;
}

/// Helper function to convert string to lowercase
fn stringToLower(str: []const u8, allocator: std.mem.Allocator) !String {
    var result = try String.init(allocator, str);
    errdefer result.deinit();
    switch (result.data) {
        .mutable => |*mutable| {
            for (mutable.items) |*ch| {
                ch.* = charToLower(ch.*);
            }
        },
        .static => unreachable,
    }
    return result;
}

/// Helper function to convert string to uppercase
fn stringToUpper(str: []const u8, allocator: std.mem.Allocator) !String {
    var result = try String.init(allocator, str);
    errdefer result.deinit(allocator);
    switch (result.data) {
        .mutable => |*mutable| {
            for (mutable.items) |*ch| {
                ch.* = charToUpper(ch.*);
            }
        },
        .static => unreachable,
    }
    return result;
}

/// Helper function to compare strings
fn stringCompare(a: []const u8, b: []const u8) std.math.Order {
    return std.mem.order(u8, a, b);
}

/// Helper function to compare strings case-insensitively
fn stringCiCompare(a: []const u8, b: []const u8) std.math.Order {
    const min_len = @min(a.len, b.len);

    for (a[0..min_len], b[0..min_len]) |ca, cb| {
        const lower_ca = charToLower(ca);
        const lower_cb = charToLower(cb);
        if (lower_ca < lower_cb) return .lt;
        if (lower_ca > lower_cb) return .gt;
    }

    return std.math.order(a.len, b.len);
}

/// Native implementation of the `string?` predicate.
///
/// Tests whether the argument is a string. Returns #t if the argument
/// is a string, #f otherwise.
///
/// Args:
///   obj: The value to test for string type
///
/// Returns:
///   #t if the argument is a string, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const string_predicate_func = NativeProc.Native{
    .name = "string?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }
            return Val.init(args[0].isString());
        }
    }.func,
};

/// Native implementation of the `make-string` function.
///
/// Creates a string of specified length, optionally filled with a character.
/// If no character is provided, the string is filled with null bytes.
///
/// Args:
///   k: The length of the string to create
///   char (optional): The character to fill the string with
///
/// Returns:
///   A new string of the specified length
///
/// Errors:
///   - `wrong-number-of-arguments`: When not 1 or 2 arguments are provided
///   - `type-error`: When k is not an integer or char is not a character
///   - `invalid-argument`: When k is negative
const make_string_func = NativeProc.Native{
    .name = "make-string",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 1 or args.len > 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const k = switch (args[0].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            if (k < 0) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const fill_char = if (args.len == 2) switch (args[1].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            } else 0;

            const len = @as(usize, @intCast(k));
            var result = try String.init(ctx.vm.allocator, "");
            errdefer result.deinit(ctx.vm.allocator);
            for (0..len) |_| {
                try result.appendByte(ctx.vm.allocator, fill_char);
            }

            return try ctx.vm.builder().build(result);
        }
    }.func,
};

/// Native implementation of the `string` function.
///
/// Creates a string from the provided character arguments.
///
/// Args:
///   char1, char2, ...: Zero or more characters to form the string
///
/// Returns:
///   A new string containing the provided characters
///
/// Errors:
///   - `type-error`: When any argument is not a character
const string_func = NativeProc.Native{
    .name = "string",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();

            var result = try String.init(ctx.vm.allocator, "");
            errdefer result.deinit(ctx.vm.allocator);
            if (args.len == 0) {
                return try ctx.vm.builder().build(result);
            }

            // Check that all arguments are characters
            for (args) |arg| {
                switch (arg.repr) {
                    .char => {},
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                }
            }
            for (args) |arg| {
                try result.appendByte(ctx.vm.allocator, arg.repr.char.data);
            }

            return try ctx.vm.builder().build(result);
        }
    }.func,
};

/// Native implementation of the `string-length` function.
///
/// Returns the length of a string in bytes.
///
/// Args:
///   string: The string to measure
///
/// Returns:
///   The length of the string as an integer
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a string
const string_length_func = NativeProc.Native{
    .name = "string-length",
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

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            return Val.init(@as(i64, @intCast(string.len())));
        }
    }.func,
};

/// Native implementation of the `string-ref` function.
///
/// Returns the character at the specified index in the string.
///
/// Args:
///   string: The string to access
///   k: The index to access (0-based)
///
/// Returns:
///   The character at the specified index
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When string is not a string or k is not an integer
///   - `invalid-argument`: When k is out of bounds
const string_ref_func = NativeProc.Native{
    .name = "string-ref",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
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

            const k = switch (args[1].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            if (k < 0 or k >= @as(i64, @intCast(string.len()))) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const index = @as(usize, @intCast(k));
            const slice = string.slice();
            return Val.init(Char.init(slice[index]));
        }
    }.func,
};

/// Native implementation of the `string-set!` function.
///
/// Sets the character at the specified index in the string.
///
/// Args:
///   string: The string to modify
///   k: The index to modify (0-based)
///   char: The character to set
///
/// Returns:
///   *unspecified*
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 3 arguments are provided
///   - `type-error`: When string is not a string, k is not an integer, or char is not a character
///   - `invalid-argument`: When k is out of bounds
const string_set_func = NativeProc.Native{
    .name = "string-set!",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 3) {
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

            const k = switch (args[1].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const ch = switch (args[2].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const string = ctx.vm.strings.getMutable(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            if (k < 0 or k >= @as(i64, @intCast(string.len()))) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const index = @as(usize, @intCast(k));
            switch (string.data) {
                .static => try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument")),
                .mutable => |*mutable| {
                    mutable.items[index] = ch;
                },
            }

            return Val.init(ctx.vm.common_symbols.@"*unspecified*");
        }
    }.func,
};

/// Native implementation of the `string=?` comparison predicate.
///
/// Tests whether all arguments are strings and are equal to each other.
/// Returns #t if all arguments are strings and equal, #f otherwise.
///
/// Args:
///   string1, string2, string3, ...: Two or more strings to compare
///
/// Returns:
///   #t if all arguments are strings and equal, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a string
const string_equal_func = NativeProc.Native{
    .name = "string=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            // Check that all arguments are strings and get their slices
            var slices = try std.ArrayList([]const u8).initCapacity(ctx.vm.allocator, args.len);
            defer slices.deinit(ctx.vm.allocator);

            for (args) |arg| {
                const string_handle = switch (arg.repr) {
                    .string => |h| h,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                };
                const string = ctx.vm.strings.get(string_handle) orelse {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                };
                try slices.append(ctx.vm.allocator, string.slice());
            }

            // Compare all strings to the first one
            const first_slice = slices.items[0];
            for (slices.items[1..]) |slice| {
                if (!std.mem.eql(u8, first_slice, slice)) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `string-ci=?` case-insensitive comparison predicate.
const string_ci_equal_func = NativeProc.Native{
    .name = "string-ci=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            // Check that all arguments are strings and get their slices
            var slices = try std.ArrayList([]const u8).initCapacity(ctx.vm.allocator, args.len);
            defer slices.deinit(ctx.vm.allocator);

            for (args) |arg| {
                const string_handle = switch (arg.repr) {
                    .string => |h| h,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                };
                const string = ctx.vm.strings.get(string_handle) orelse {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                };
                try slices.append(ctx.vm.allocator, string.slice());
            }

            // Compare all strings to the first one (case-insensitive)
            const first_slice = slices.items[0];
            for (slices.items[1..]) |slice| {
                if (stringCiCompare(first_slice, slice) != .eq) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `string<?` comparison predicate.
const string_less_func = NativeProc.Native{
    .name = "string<?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            // Check that all arguments are strings and get their slices
            var slices = try std.ArrayList([]const u8).initCapacity(ctx.vm.allocator, args.len);
            defer slices.deinit(ctx.vm.allocator);

            for (args) |arg| {
                const string_handle = switch (arg.repr) {
                    .string => |h| h,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                };
                const string = ctx.vm.strings.get(string_handle) orelse {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                };
                try slices.append(ctx.vm.allocator, string.slice());
            }

            // Check strictly increasing order
            for (slices.items[0 .. slices.items.len - 1], slices.items[1..]) |left, right| {
                if (stringCompare(left, right) != .lt) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `string-ci<?` case-insensitive comparison predicate.
const string_ci_less_func = NativeProc.Native{
    .name = "string-ci<?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            // Check that all arguments are strings and get their slices
            var slices = try std.ArrayList([]const u8).initCapacity(ctx.vm.allocator, args.len);
            defer slices.deinit(ctx.vm.allocator);

            for (args) |arg| {
                const string_handle = switch (arg.repr) {
                    .string => |h| h,
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                };
                const string = ctx.vm.strings.get(string_handle) orelse {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                };
                try slices.append(ctx.vm.allocator, string.slice());
            }

            // Check strictly increasing order (case-insensitive)
            for (slices.items[0 .. slices.items.len - 1], slices.items[1..]) |left, right| {
                if (stringCiCompare(left, right) != .lt) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

// Helper macro to reduce code duplication for remaining comparison functions
fn createStringComparisonFunc(comptime name: []const u8, comptime op: enum { gt, le, ge }, comptime case_insensitive: bool) NativeProc.Native {
    return NativeProc.Native{
        .name = name,
        .func = struct {
            fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
                const args = ctx.localStack();
                if (args.len < 2) {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                }

                // Check that all arguments are strings and get their slices
                var slices = try std.ArrayList([]const u8).initCapacity(ctx.vm.allocator, args.len);
                defer slices.deinit(ctx.vm.allocator);

                for (args) |arg| {
                    const string_handle = switch (arg.repr) {
                        .string => |h| h,
                        else => {
                            try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                            return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                        },
                    };
                    const string = ctx.vm.strings.get(string_handle) orelse {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    };
                    try slices.append(ctx.vm.allocator, string.slice());
                }

                // Check the appropriate comparison order
                for (slices.items[0 .. slices.items.len - 1], slices.items[1..]) |left, right| {
                    const order = if (case_insensitive) stringCiCompare(left, right) else stringCompare(left, right);
                    const result = switch (op) {
                        .gt => order == .gt,
                        .le => order == .lt or order == .eq,
                        .ge => order == .gt or order == .eq,
                    };
                    if (!result) {
                        return Val.init(false);
                    }
                }

                return Val.init(true);
            }
        }.func,
    };
}

/// Native implementation of the `string>?` comparison predicate.
const string_greater_func = createStringComparisonFunc("string>?", .gt, false);

/// Native implementation of the `string-ci>?` case-insensitive comparison predicate.
const string_ci_greater_func = createStringComparisonFunc("string-ci>?", .gt, true);

/// Native implementation of the `string<=?` comparison predicate.
const string_less_equal_func = createStringComparisonFunc("string<=?", .le, false);

/// Native implementation of the `string-ci<=?` case-insensitive comparison predicate.
const string_ci_less_equal_func = createStringComparisonFunc("string-ci<=?", .le, true);

/// Native implementation of the `string>=?` comparison predicate.
const string_greater_equal_func = createStringComparisonFunc("string>=?", .ge, false);

/// Native implementation of the `string-ci>=?` case-insensitive comparison predicate.
const string_ci_greater_equal_func = createStringComparisonFunc("string-ci>=?", .ge, true);

/// Native implementation of the `string-upcase` conversion function.
///
/// Converts a string to uppercase.
/// Returns a new string with all lowercase letters converted to uppercase.
///
/// Args:
///   string: The string to convert to uppercase
///
/// Returns:
///   Uppercase version of the string
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a string
const string_upcase_func = NativeProc.Native{
    .name = "string-upcase",
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

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const slice = string.slice();

            const uppercase_string = try stringToUpper(slice, ctx.vm.allocator);
            return try ctx.vm.builder().build(uppercase_string);
        }
    }.func,
};

/// Native implementation of the `string-downcase` conversion function.
///
/// Converts a string to lowercase.
/// Returns a new string with all uppercase letters converted to lowercase.
///
/// Args:
///   string: The string to convert to lowercase
///
/// Returns:
///   Lowercase version of the string
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a string
const string_downcase_func = NativeProc.Native{
    .name = "string-downcase",
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

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const slice = string.slice();

            const lowercase_string = try stringToLower(slice, ctx.vm.allocator);
            return try ctx.vm.builder().build(lowercase_string);
        }
    }.func,
};

/// Native implementation of the `string-foldcase` conversion function.
///
/// Converts a string to its case-folded form (lowercase for ASCII).
/// This is equivalent to string-downcase for ASCII characters.
///
/// Args:
///   string: The string to case-fold
///
/// Returns:
///   Case-folded version of the string
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a string
const string_foldcase_func = NativeProc.Native{
    .name = "string-foldcase",
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

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const slice = string.slice();

            var folded_string = try stringToLower(slice, ctx.vm.allocator);
            defer folded_string.deinit(ctx.vm.allocator);

            return try ctx.vm.builder().build(folded_string);
        }
    }.func,
};

/// Native implementation of the `substring` function.
///
/// Extracts a substring from the given string between start and end indices.
///
/// Args:
///   string: The source string
///   start: The starting index (inclusive)
///   end: The ending index (exclusive)
///
/// Returns:
///   A new string containing the specified substring
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 3 arguments are provided
///   - `type-error`: When arguments have wrong types
///   - `invalid-argument`: When indices are out of bounds or start > end
const substring_func = NativeProc.Native{
    .name = "substring",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 3) {
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

            const start = switch (args[1].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const end = switch (args[2].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const slice = string.slice();
            const len = @as(i64, @intCast(slice.len));

            if (start < 0 or end < 0 or start > len or end > len or start > end) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const start_idx = @as(usize, @intCast(start));
            const end_idx = @as(usize, @intCast(end));
            const substr = slice[start_idx..end_idx];

            return try ctx.vm.builder().build(try String.init(ctx.vm.allocator, substr));
        }
    }.func,
};

/// Native implementation of the `string-append` function.
///
/// Concatenates multiple strings into a single string.
///
/// Args:
///   string1, string2, ...: Zero or more strings to concatenate
///
/// Returns:
///   A new string containing all the input strings concatenated
///
/// Errors:
///   - `type-error`: When any argument is not a string
const string_append_func = NativeProc.Native{
    .name = "string-append",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();

            var result = try String.init(ctx.vm.allocator, "");
            errdefer result.deinit(ctx.vm.allocator);

            // Check that all arguments are strings
            for (args) |arg| {
                switch (arg.repr) {
                    .string => {},
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                }
            }

            for (args) |arg| {
                const string_handle = arg.repr.string;
                const string = ctx.vm.strings.get(string_handle) orelse {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                };
                try result.appendSlice(ctx.vm.allocator, string.slice());
            }

            return try ctx.vm.builder().build(result);
        }
    }.func,
};

/// Native implementation of the `string-copy` function.
///
/// Creates a copy of a string.
///
/// Args:
///   string: The string to copy
///
/// Returns:
///   A new string with the same content
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a string
const string_copy_func = NativeProc.Native{
    .name = "string-copy",
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

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const slice = string.slice();
            var result = try String.init(ctx.vm.allocator, slice);
            errdefer result.deinit(ctx.vm.allocator);
            return try ctx.vm.builder().build(result);
        }
    }.func,
};

/// Native implementation of the `string-copy!` function.
///
/// Copies the contents of one string into another.
///
/// Args:
///   to: The destination string
///   at: The starting index in the destination
///   from: The source string
///   start (optional): The starting index in the source (default 0)
///   end (optional): The ending index in the source (default source length)
///
/// Returns:
///   *unspecified*
///
/// Errors:
///   - `wrong-number-of-arguments`: When not 3-5 arguments are provided
///   - `type-error`: When arguments have wrong types
///   - `invalid-argument`: When indices are out of bounds
const string_copy_bang_func = NativeProc.Native{
    .name = "string-copy!",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 3 or args.len > 5) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const to_handle = switch (args[0].repr) {
                .string => |h| h,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const at = switch (args[1].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const from_handle = switch (args[2].repr) {
                .string => |h| h,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const from_string = ctx.vm.strings.get(from_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const from_slice = from_string.slice();

            const start = if (args.len >= 4) switch (args[3].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            } else 0;

            const end = if (args.len >= 5) switch (args[4].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            } else @as(i64, @intCast(from_slice.len));

            // Validate indices
            const from_len = @as(i64, @intCast(from_slice.len));
            if (start < 0 or end < 0 or start > from_len or end > from_len or start > end) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            if (at < 0) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const to_string = ctx.vm.strings.getMutable(to_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const copy_len = @as(usize, @intCast(end - start));
            const at_idx = @as(usize, @intCast(at));

            if (at_idx + copy_len > to_string.len()) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const start_idx = @as(usize, @intCast(start));
            const end_idx = @as(usize, @intCast(end));
            const copy_slice = from_slice[start_idx..end_idx];

            // Ensure destination string is mutable
            switch (to_string.data) {
                .static => |static| {
                    var mutable = std.ArrayList(u8){};
                    try mutable.appendSlice(ctx.vm.allocator, static);
                    @memcpy(mutable.items[at_idx .. at_idx + copy_len], copy_slice);
                    to_string.data = .{ .mutable = mutable };
                },
                .mutable => |*mutable| {
                    @memcpy(mutable.items[at_idx .. at_idx + copy_len], copy_slice);
                },
            }

            return Val.init(ctx.vm.common_symbols.@"*unspecified*");
        }
    }.func,
};

/// Native implementation of the `string-fill!` function.
///
/// Fills a string with a specified character.
///
/// Args:
///   string: The string to fill
///   char: The character to fill with
///   start (optional): The starting index (default 0)
///   end (optional): The ending index (default string length)
///
/// Returns:
///   *unspecified*
///
/// Errors:
///   - `wrong-number-of-arguments`: When not 2-4 arguments are provided
///   - `type-error`: When arguments have wrong types
///   - `invalid-argument`: When indices are out of bounds
const string_fill_bang_func = NativeProc.Native{
    .name = "string-fill!",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2 or args.len > 4) {
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

            const ch = switch (args[1].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const string = ctx.vm.strings.getMutable(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const len = @as(i64, @intCast(string.len()));

            const start = if (args.len >= 3) switch (args[2].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            } else 0;

            const end = if (args.len >= 4) switch (args[3].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            } else len;

            // Validate indices
            if (start < 0 or end < 0 or start > len or end > len or start > end) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const start_idx = @as(usize, @intCast(start));
            const end_idx = @as(usize, @intCast(end));

            // Ensure string is mutable
            switch (string.data) {
                .static => |static| {
                    var mutable = std.ArrayList(u8){};
                    try mutable.appendSlice(ctx.vm.allocator, static);
                    @memset(mutable.items[start_idx..end_idx], ch);
                    string.data = .{ .mutable = mutable };
                },
                .mutable => |*mutable| {
                    @memset(mutable.items[start_idx..end_idx], ch);
                },
            }

            return Val.init(ctx.vm.common_symbols.@"*unspecified*");
        }
    }.func,
};

/// Native implementation of the `string->list` function.
///
/// Converts a string to a list of characters.
///
/// Args:
///   string: The string to convert
///   start (optional): The starting index (default 0)
///   end (optional): The ending index (default string length)
///
/// Returns:
///   A list containing the characters from the string
///
/// Errors:
///   - `wrong-number-of-arguments`: When not 1-3 arguments are provided
///   - `type-error`: When arguments have wrong types
///   - `invalid-argument`: When indices are out of bounds
const string_to_list_func = NativeProc.Native{
    .name = "string->list",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 1 or args.len > 3) {
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

            const string = ctx.vm.strings.get(string_handle) orelse {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            };
            const slice = string.slice();
            const len = @as(i64, @intCast(slice.len));

            const start = if (args.len >= 2) switch (args[1].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            } else 0;

            const end = if (args.len >= 3) switch (args[2].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            } else len;

            // Validate indices
            if (start < 0 or end < 0 or start > len or end > len or start > end) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const start_idx = @as(usize, @intCast(start));
            const end_idx = @as(usize, @intCast(end));

            // Build list from characters
            var result = Val{ .repr = .nil };

            // Build list in reverse to avoid needing to reverse later
            var i = end_idx;
            while (i > start_idx) {
                i -= 1;
                const ch = Val.init(Char.init(slice[i]));
                const pair = try ctx.vm.builder().build(Pair.init(ch, result));
                result = pair;
            }

            return result;
        }
    }.func,
};

/// Native implementation of the `list->string` function.
///
/// Converts a list of characters to a string.
///
/// Args:
///   list: The list of characters to convert
///
/// Returns:
///   A string containing the characters from the list
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a list or contains non-character elements
const list_to_string_func = NativeProc.Native{
    .name = "list->string",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            var result = try String.init(ctx.vm.allocator, "");
            errdefer result.deinit(ctx.vm.allocator);
            var current = args[0];
            while (true) {
                switch (current.repr) {
                    .nil => break,
                    .pair => |pair_handle| {
                        const pair = ctx.vm.pairs.get(pair_handle) orelse {
                            try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                            return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                        };
                        const ch = switch (pair.car.repr) {
                            .char => |c| c.data,
                            else => {
                                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                            },
                        };
                        try result.appendByte(ctx.vm.allocator, ch);
                        current = pair.cdr;
                    },
                    else => {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                    },
                }
            }

            return try ctx.vm.builder().build(result);
        }
    }.func,
};

// Tests
test "string? returns #t for strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(string? \"hello\")");
    try vm.expectEval("#t", "(string? \"\")");
}

test "string? returns #f for non-strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(string? 42)");
    try vm.expectEval("#f", "(string? #t)");
    try vm.expectEval("#f", "(string? #\\a)");
    try vm.expectEval("#f", "(string? '())");
}

test "make-string creates strings of specified length" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"\"", "(make-string 0)");
    try vm.expectEval("3", "(string-length (make-string 3))");
    try vm.expectEval("\"aaa\"", "(make-string 3 #\\a)");
}

test "string creates strings from characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"\"", "(string)");
    try vm.expectEval("\"a\"", "(string #\\a)");
    try vm.expectEval("\"hello\"", "(string #\\h #\\e #\\l #\\l #\\o)");
}

test "string-length returns correct length" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(string-length \"\")");
    try vm.expectEval("5", "(string-length \"hello\")");
    try vm.expectEval("1", "(string-length \"a\")");
}

test "string-ref accesses characters correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\h", "(string-ref \"hello\" 0)");
    try vm.expectEval("#\\e", "(string-ref \"hello\" 1)");
    try vm.expectEval("#\\o", "(string-ref \"hello\" 4)");
}

test "string=? compares strings correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(string=? \"hello\" \"hello\")");
    try vm.expectEval("#f", "(string=? \"hello\" \"world\")");
    try vm.expectEval("#t", "(string=? \"\" \"\")");
    try vm.expectEval("#t", "(string=? \"a\" \"a\" \"a\")");
    try vm.expectEval("#f", "(string=? \"a\" \"a\" \"b\")");
}

test "string-ci=? compares strings case-insensitively" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(string-ci=? \"Hello\" \"hello\")");
    try vm.expectEval("#t", "(string-ci=? \"HELLO\" \"hello\")");
    try vm.expectEval("#f", "(string-ci=? \"hello\" \"world\")");
}

test "string<? compares strings lexicographically" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(string<? \"a\" \"b\")");
    try vm.expectEval("#t", "(string<? \"apple\" \"banana\")");
    try vm.expectEval("#f", "(string<? \"b\" \"a\")");
    try vm.expectEval("#f", "(string<? \"hello\" \"hello\")");
}

test "string-upcase converts to uppercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"HELLO\"", "(string-upcase \"hello\")");
    try vm.expectEval("\"HELLO\"", "(string-upcase \"HELLO\")");
    try vm.expectEval("\"ABC123\"", "(string-upcase \"abc123\")");
}

test "string-downcase converts to lowercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"hello\"", "(string-downcase \"HELLO\")");
    try vm.expectEval("\"hello\"", "(string-downcase \"hello\")");
    try vm.expectEval("\"abc123\"", "(string-downcase \"ABC123\")");
}

test "substring extracts substrings correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"hello\"", "(substring \"hello world\" 0 5)");
    try vm.expectEval("\"world\"", "(substring \"hello world\" 6 11)");
    try vm.expectEval("\"\"", "(substring \"hello\" 2 2)");
}

test "string-append concatenates strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"\"", "(string-append)");
    try vm.expectEval("\"hello\"", "(string-append \"hello\")");
    try vm.expectEval("\"hello world\"", "(string-append \"hello\" \" \" \"world\")");
}

test "string-copy creates string copies" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"hello\"", "(string-copy \"hello\")");
    try vm.expectEval("\"\"", "(string-copy \"\")");
}

test "string->list converts strings to character lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(string->list \"\")");
    try vm.expectEval("(#\\a)", "(string->list \"a\")");
    try vm.expectEval("(#\\h #\\i)", "(string->list \"hi\")");
}

test "list->string converts character lists to strings" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("\"\"", "(list->string '())");
    try vm.expectEval("\"a\"", "(list->string '(#\\a))");
    try vm.expectEval("\"hi\"", "(list->string '(#\\h #\\i))");
}

test "round-trip string->list and list->string" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(string=? \"hello\" (list->string (string->list \"hello\")))");
    try vm.expectEval("#t", "(equal? '(#\\a #\\b) (string->list (list->string '(#\\a #\\b))))");
}
