//! Character operations for the Scheme interpreter.
//!
//! This module provides native implementations of character predicates,
//! comparison functions, case conversion, and character classification
//! operations according to the R7RS Scheme specification.

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const NativeProc = @import("../NativeProc.zig");
const Proc = @import("../Proc.zig");
const Char = @import("../types/Char.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// Registers all character functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register character functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&char_predicate_func);
    try vm.builder().defineNativeProc(&char_equal_func);
    try vm.builder().defineNativeProc(&char_less_func);
    try vm.builder().defineNativeProc(&char_greater_func);
    try vm.builder().defineNativeProc(&char_less_equal_func);
    try vm.builder().defineNativeProc(&char_greater_equal_func);
    try vm.builder().defineNativeProc(&char_ci_equal_func);
    try vm.builder().defineNativeProc(&char_ci_less_func);
    try vm.builder().defineNativeProc(&char_ci_greater_func);
    try vm.builder().defineNativeProc(&char_ci_less_equal_func);
    try vm.builder().defineNativeProc(&char_ci_greater_equal_func);
    try vm.builder().defineNativeProc(&char_alphabetic_func);
    try vm.builder().defineNativeProc(&char_numeric_func);
    try vm.builder().defineNativeProc(&char_whitespace_func);
    try vm.builder().defineNativeProc(&char_upper_case_func);
    try vm.builder().defineNativeProc(&char_lower_case_func);
    try vm.builder().defineNativeProc(&digit_value_func);
    try vm.builder().defineNativeProc(&char_to_integer_func);
    try vm.builder().defineNativeProc(&integer_to_char_func);
    try vm.builder().defineNativeProc(&char_upcase_func);
    try vm.builder().defineNativeProc(&char_downcase_func);
    try vm.builder().defineNativeProc(&char_foldcase_func);
}

/// Native implementation of the `char?` predicate.
///
/// Tests whether the argument is a character. Returns #t if the argument
/// is a character, #f otherwise.
///
/// Args:
///   obj: The value to test for character type
///
/// Returns:
///   #t if the argument is a character, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const char_predicate_func = NativeProc.Native{
    .name = "char?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }
            return Val.init(args[0].isChar());
        }
    }.func,
};

/// Native implementation of the `char=?` comparison predicate.
///
/// Tests whether all arguments are characters and are equal to each other.
/// Returns #t if all arguments are characters and equal, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and equal, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_equal_func = NativeProc.Native{
    .name = "char=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Compare all characters to the first one
            const first_char = args[0].repr.char.data;
            for (args[1..]) |arg| {
                const current_char = arg.repr.char.data;
                if (first_char != current_char) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char<?` comparison predicate.
///
/// Tests whether arguments are characters in strictly increasing order.
/// Returns #t if all arguments are characters and in increasing order, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in increasing order, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_less_func = NativeProc.Native{
    .name = "char<?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check strictly increasing order
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                if (left.repr.char.data >= right.repr.char.data) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char>?` comparison predicate.
///
/// Tests whether arguments are characters in strictly decreasing order.
/// Returns #t if all arguments are characters and in decreasing order, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in decreasing order, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_greater_func = NativeProc.Native{
    .name = "char>?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check strictly decreasing order
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                if (left.repr.char.data <= right.repr.char.data) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char<=?` comparison predicate.
///
/// Tests whether arguments are characters in non-decreasing order.
/// Returns #t if all arguments are characters and in non-decreasing order, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in non-decreasing order, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_less_equal_func = NativeProc.Native{
    .name = "char<=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check non-decreasing order
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                if (left.repr.char.data > right.repr.char.data) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char>=?` comparison predicate.
///
/// Tests whether arguments are characters in non-increasing order.
/// Returns #t if all arguments are characters and in non-increasing order, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in non-increasing order, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_greater_equal_func = NativeProc.Native{
    .name = "char>=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check non-increasing order
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                if (left.repr.char.data < right.repr.char.data) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Helper function to convert character to lowercase for case-insensitive comparison
fn charToLower(ch: u8) u8 {
    const delta = 'a' - 'A';
    return if (ch >= 'A' and ch <= 'Z') ch + delta else ch;
}

/// Helper function to convert character to uppercase
fn charToUpper(ch: u8) u8 {
    const delta = 'a' - 'A';
    return if (ch >= 'a' and ch <= 'z') ch - delta else ch;
}

/// Native implementation of the `char-ci=?` case-insensitive comparison predicate.
///
/// Tests whether all arguments are characters and are equal when case is ignored.
/// Returns #t if all arguments are characters and equal ignoring case, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and equal ignoring case, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_ci_equal_func = NativeProc.Native{
    .name = "char-ci=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Compare all characters to the first one (case-insensitive)
            const first_char = charToLower(args[0].repr.char.data);
            for (args[1..]) |arg| {
                const current_char = charToLower(arg.repr.char.data);
                if (first_char != current_char) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char-ci<?` case-insensitive comparison predicate.
///
/// Tests whether arguments are characters in strictly increasing order when case is ignored.
/// Returns #t if all arguments are characters and in increasing order ignoring case, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in increasing order ignoring case, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_ci_less_func = NativeProc.Native{
    .name = "char-ci<?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check strictly increasing order (case-insensitive)
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                const left_char = charToLower(left.repr.char.data);
                const right_char = charToLower(right.repr.char.data);
                if (left_char >= right_char) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char-ci>?` case-insensitive comparison predicate.
///
/// Tests whether arguments are characters in strictly decreasing order when case is ignored.
/// Returns #t if all arguments are characters and in decreasing order ignoring case, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in decreasing order ignoring case, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_ci_greater_func = NativeProc.Native{
    .name = "char-ci>?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check strictly decreasing order (case-insensitive)
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                const left_char = charToLower(left.repr.char.data);
                const right_char = charToLower(right.repr.char.data);
                if (left_char <= right_char) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char-ci<=?` case-insensitive comparison predicate.
///
/// Tests whether arguments are characters in non-decreasing order when case is ignored.
/// Returns #t if all arguments are characters and in non-decreasing order ignoring case, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in non-decreasing order ignoring case, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_ci_less_equal_func = NativeProc.Native{
    .name = "char-ci<=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check non-decreasing order (case-insensitive)
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                const left_char = charToLower(left.repr.char.data);
                const right_char = charToLower(right.repr.char.data);
                if (left_char > right_char) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char-ci>=?` case-insensitive comparison predicate.
///
/// Tests whether arguments are characters in non-increasing order when case is ignored.
/// Returns #t if all arguments are characters and in non-increasing order ignoring case, #f otherwise.
///
/// Args:
///   char1, char2, char3, ...: Two or more characters to compare
///
/// Returns:
///   #t if all arguments are characters and in non-increasing order ignoring case, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When fewer than 2 arguments are provided
///   - `type-error`: When any argument is not a character
const char_ci_greater_equal_func = NativeProc.Native{
    .name = "char-ci>=?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len < 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
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

            // Check non-increasing order (case-insensitive)
            for (args[0 .. args.len - 1], args[1..]) |left, right| {
                const left_char = charToLower(left.repr.char.data);
                const right_char = charToLower(right.repr.char.data);
                if (left_char < right_char) {
                    return Val.init(false);
                }
            }

            return Val.init(true);
        }
    }.func,
};

/// Native implementation of the `char-alphabetic?` predicate.
///
/// Tests whether the argument is an alphabetic character (a-z, A-Z).
/// Returns #t if the argument is alphabetic, #f otherwise.
///
/// Args:
///   char: The character to test
///
/// Returns:
///   #t if the argument is alphabetic, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_alphabetic_func = NativeProc.Native{
    .name = "char-alphabetic?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const is_alphabetic = (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z');
            return Val.init(is_alphabetic);
        }
    }.func,
};

/// Native implementation of the `char-numeric?` predicate.
///
/// Tests whether the argument is a numeric character (0-9).
/// Returns #t if the argument is numeric, #f otherwise.
///
/// Args:
///   char: The character to test
///
/// Returns:
///   #t if the argument is numeric, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_numeric_func = NativeProc.Native{
    .name = "char-numeric?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const is_numeric = ch >= '0' and ch <= '9';
            return Val.init(is_numeric);
        }
    }.func,
};

/// Native implementation of the `char-whitespace?` predicate.
///
/// Tests whether the argument is a whitespace character.
/// Recognized whitespace characters are: space (#\space), tab (#\tab),
/// newline (#\newline), carriage return (#\return), vertical tab, and form feed.
/// Returns #t if the argument is whitespace, #f otherwise.
///
/// Args:
///   char: The character to test
///
/// Returns:
///   #t if the argument is whitespace, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_whitespace_func = NativeProc.Native{
    .name = "char-whitespace?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const is_whitespace = ch == ' ' or ch == '\t' or ch == '\n' or ch == '\r' or ch == '\x0B' or ch == '\x0C';
            return Val.init(is_whitespace);
        }
    }.func,
};

/// Native implementation of the `char-upper-case?` predicate.
///
/// Tests whether the argument is an uppercase alphabetic character (A-Z).
/// Returns #t if the argument is uppercase, #f otherwise.
///
/// Args:
///   char: The character to test
///
/// Returns:
///   #t if the argument is uppercase, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_upper_case_func = NativeProc.Native{
    .name = "char-upper-case?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const is_upper = ch >= 'A' and ch <= 'Z';
            return Val.init(is_upper);
        }
    }.func,
};

/// Native implementation of the `char-lower-case?` predicate.
///
/// Tests whether the argument is a lowercase alphabetic character (a-z).
/// Returns #t if the argument is lowercase, #f otherwise.
///
/// Args:
///   char: The character to test
///
/// Returns:
///   #t if the argument is lowercase, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_lower_case_func = NativeProc.Native{
    .name = "char-lower-case?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const is_lower = ch >= 'a' and ch <= 'z';
            return Val.init(is_lower);
        }
    }.func,
};

/// Native implementation of the `digit-value` function.
///
/// Returns the numeric value of a digit character (0-9).
/// Returns #f if the character is not a digit.
///
/// Args:
///   char: The character to get the digit value of
///
/// Returns:
///   Integer from 0-9 if the character is a digit, #f otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const digit_value_func = NativeProc.Native{
    .name = "digit-value",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            if (ch >= '0' and ch <= '9') {
                return Val.init(@as(i64, ch - '0'));
            } else {
                return Val.init(false);
            }
        }
    }.func,
};

/// Native implementation of the `char->integer` conversion function.
///
/// Converts a character to its ASCII/Unicode integer value.
/// Returns the integer value of the character.
///
/// Args:
///   char: The character to convert to an integer
///
/// Returns:
///   Integer value of the character
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_to_integer_func = NativeProc.Native{
    .name = "char->integer",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            return Val.init(@as(i64, ch));
        }
    }.func,
};

/// Native implementation of the `integer->char` conversion function.
///
/// Converts an integer to its corresponding character value.
/// Returns the character represented by the integer.
///
/// Args:
///   n: The integer to convert to a character
///
/// Returns:
///   Character value corresponding to the integer
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not an integer
///   - `invalid-argument`: When the integer is not in the valid character range
const integer_to_char_func = NativeProc.Native{
    .name = "integer->char",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const n = switch (args[0].repr) {
                .i64 => |i| i,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            if (n < 0 or n > 255) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            return Val.init(Char.init(@as(u8, @intCast(n))));
        }
    }.func,
};

/// Native implementation of the `char-upcase` conversion function.
///
/// Converts a character to uppercase if it's a lowercase letter.
/// Returns the uppercase version of the character, or the character unchanged.
///
/// Args:
///   char: The character to convert to uppercase
///
/// Returns:
///   Uppercase version of the character
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_upcase_func = NativeProc.Native{
    .name = "char-upcase",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const uppercase_ch = charToUpper(ch);
            return Val.init(Char.init(uppercase_ch));
        }
    }.func,
};

/// Native implementation of the `char-downcase` conversion function.
///
/// Converts a character to lowercase if it's an uppercase letter.
/// Returns the lowercase version of the character, or the character unchanged.
///
/// Args:
///   char: The character to convert to lowercase
///
/// Returns:
///   Lowercase version of the character
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_downcase_func = NativeProc.Native{
    .name = "char-downcase",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const lowercase_ch = charToLower(ch);
            return Val.init(Char.init(lowercase_ch));
        }
    }.func,
};

/// Native implementation of the `char-foldcase` conversion function.
///
/// Converts a character to its case-folded form (lowercase for ASCII).
/// This is equivalent to char-downcase for ASCII characters.
///
/// Args:
///   char: The character to case-fold
///
/// Returns:
///   Case-folded version of the character
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a character
const char_foldcase_func = NativeProc.Native{
    .name = "char-foldcase",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init(ctx.vm.common_symbols.@"*unspecified*");
            }

            const ch = switch (args[0].repr) {
                .char => |c| c.data,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
            };

            const folded_ch = charToLower(ch);
            return Val.init(Char.init(folded_ch));
        }
    }.func,
};

// Tests
test "char? returns #t for characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char? #\\a)");
    try vm.expectEval("#t", "(char? #\\space)");
    try vm.expectEval("#t", "(char? #\\0)");
}

test "char? returns #f for non-characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char? 42)");
    try vm.expectEval("#f", "(char? #t)");
    try vm.expectEval("#f", "(char? \"hello\")");
    try vm.expectEval("#f", "(char? 'symbol)");
}

test "char? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(char?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(char? #\\a #\\b)"),
    );
}

test "char=? returns #t for equal characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char=? #\\a #\\a)");
    try vm.expectEval("#t", "(char=? #\\space #\\space #\\space)");
    try vm.expectEval("#t", "(char=? #\\0 #\\0 #\\0 #\\0)");
}

test "char=? returns #f for different characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char=? #\\a #\\b)");
    try vm.expectEval("#f", "(char=? #\\a #\\a #\\b)");
    try vm.expectEval("#f", "(char=? #\\A #\\a)");
}

test "char<? returns #t for characters in increasing order" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char<? #\\a #\\b)");
    try vm.expectEval("#t", "(char<? #\\a #\\b #\\c)");
    try vm.expectEval("#t", "(char<? #\\0 #\\1 #\\2)");
}

test "char<? returns #f for characters not in increasing order" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(char<? #\\b #\\a)");
    try vm.expectEval("#f", "(char<? #\\a #\\a)");
    try vm.expectEval("#f", "(char<? #\\a #\\c #\\b)");
}

test "char>? returns #t for characters in decreasing order" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char>? #\\b #\\a)");
    try vm.expectEval("#t", "(char>? #\\c #\\b #\\a)");
    try vm.expectEval("#t", "(char>? #\\2 #\\1 #\\0)");
}

test "char<=? returns #t for characters in non-decreasing order" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char<=? #\\a #\\b)");
    try vm.expectEval("#t", "(char<=? #\\a #\\a)");
    try vm.expectEval("#t", "(char<=? #\\a #\\a #\\b)");
}

test "char>=? returns #t for characters in non-increasing order" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char>=? #\\b #\\a)");
    try vm.expectEval("#t", "(char>=? #\\a #\\a)");
    try vm.expectEval("#t", "(char>=? #\\b #\\a #\\a)");
}

test "char-ci=? returns #t for case-insensitive equal characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-ci=? #\\a #\\A)");
    try vm.expectEval("#t", "(char-ci=? #\\A #\\a)");
    try vm.expectEval("#t", "(char-ci=? #\\a #\\A #\\a)");
}

test "char-ci<? returns #t for case-insensitive increasing order" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-ci<? #\\a #\\B)");
    try vm.expectEval("#t", "(char-ci<? #\\A #\\b)");
    try vm.expectEval("#f", "(char-ci<? #\\a #\\A)");
}

test "char-alphabetic? returns #t for alphabetic characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-alphabetic? #\\a)");
    try vm.expectEval("#t", "(char-alphabetic? #\\Z)");
    try vm.expectEval("#f", "(char-alphabetic? #\\0)");
    try vm.expectEval("#f", "(char-alphabetic? #\\space)");
}

test "char-numeric? returns #t for numeric characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-numeric? #\\0)");
    try vm.expectEval("#t", "(char-numeric? #\\9)");
    try vm.expectEval("#f", "(char-numeric? #\\a)");
    try vm.expectEval("#f", "(char-numeric? #\\space)");
}

test "char-whitespace? returns #t for whitespace characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-whitespace? #\\space)");
    try vm.expectEval("#t", "(char-whitespace? #\\tab)");
    try vm.expectEval("#t", "(char-whitespace? #\\newline)");
    try vm.expectEval("#f", "(char-whitespace? #\\a)");
}

test "char-upper-case? returns #t for uppercase characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-upper-case? #\\A)");
    try vm.expectEval("#t", "(char-upper-case? #\\Z)");
    try vm.expectEval("#f", "(char-upper-case? #\\a)");
    try vm.expectEval("#f", "(char-upper-case? #\\0)");
}

test "char-lower-case? returns #t for lowercase characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char-lower-case? #\\a)");
    try vm.expectEval("#t", "(char-lower-case? #\\z)");
    try vm.expectEval("#f", "(char-lower-case? #\\A)");
    try vm.expectEval("#f", "(char-lower-case? #\\0)");
}

test "digit-value returns correct values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(digit-value #\\0)");
    try vm.expectEval("5", "(digit-value #\\5)");
    try vm.expectEval("9", "(digit-value #\\9)");
    try vm.expectEval("#f", "(digit-value #\\a)");
    try vm.expectEval("#f", "(digit-value #\\space)");
}

test "char->integer converts characters to integers" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("65", "(char->integer #\\A)");
    try vm.expectEval("97", "(char->integer #\\a)");
    try vm.expectEval("48", "(char->integer #\\0)");
    try vm.expectEval("32", "(char->integer #\\space)");
}

test "integer->char converts integers to characters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\A", "(integer->char 65)");
    try vm.expectEval("#\\a", "(integer->char 97)");
    try vm.expectEval("#\\0", "(integer->char 48)");
    try vm.expectEval("#\\space", "(integer->char 32)");
}

test "integer->char with invalid range is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(integer->char -1)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(integer->char 256)"),
    );
}

test "char-upcase converts to uppercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\A", "(char-upcase #\\a)");
    try vm.expectEval("#\\Z", "(char-upcase #\\z)");
    try vm.expectEval("#\\A", "(char-upcase #\\A)");
    try vm.expectEval("#\\0", "(char-upcase #\\0)");
}

test "char-downcase converts to lowercase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\a", "(char-downcase #\\A)");
    try vm.expectEval("#\\z", "(char-downcase #\\Z)");
    try vm.expectEval("#\\a", "(char-downcase #\\a)");
    try vm.expectEval("#\\0", "(char-downcase #\\0)");
}

test "char-foldcase is equivalent to char-downcase" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#\\a", "(char-foldcase #\\A)");
    try vm.expectEval("#\\z", "(char-foldcase #\\Z)");
    try vm.expectEval("#\\a", "(char-foldcase #\\a)");
    try vm.expectEval("#\\0", "(char-foldcase #\\0)");
}

test "round-trip char->integer and integer->char" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(char=? #\\A (integer->char (char->integer #\\A)))");
    try vm.expectEval("#t", "(char=? #\\z (integer->char (char->integer #\\z)))");
    try vm.expectEval("#t", "(= 65 (char->integer (integer->char 65)))");
}
