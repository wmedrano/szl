//! Pair operations for the Scheme interpreter.
//!
//! This module provides native implementations of pair operations
//! including type checking (pair?), construction (cons), and accessors (car, cdr).

const std = @import("std");
const testing = std.testing;

const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const NativeProc = @import("../NativeProc.zig");
const Proc = @import("../Proc.zig");
const Pair = @import("../types/Pair.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");
const equivalence = @import("equivalence.zig");

/// Registers all pair functions with the virtual machine.
///
/// Args:
///   vm: Pointer to the VM instance to register pair functions with.
///
/// Errors:
///   May return allocation errors if registering functions fails.
pub fn register(vm: *Vm) !void {
    try vm.builder().defineNativeProc(&pair_predicate);
    try vm.builder().defineNativeProc(&cons);
    try vm.builder().defineNativeProc(&car);
    try vm.builder().defineNativeProc(&cdr);
    try vm.builder().defineNativeProc(&set_car);
    try vm.builder().defineNativeProc(&set_cdr);
    try vm.builder().defineNativeProc(&null_predicate);
    try vm.builder().defineNativeProc(&list_predicate);
    try vm.builder().defineNativeProc(&make_list);
    try vm.builder().defineNativeProc(&list);
    try vm.builder().defineNativeProc(&length);
    try vm.builder().defineNativeProc(&append);
    try vm.builder().defineNativeProc(&reverse);
    try vm.builder().defineNativeProc(&list_tail);
    try vm.builder().defineNativeProc(&list_ref);
    try vm.builder().defineNativeProc(&list_set);
    try vm.builder().defineNativeProc(&memq);
    try vm.builder().defineNativeProc(&memv);
    try vm.builder().defineNativeProc(&member);
    try vm.builder().defineNativeProc(&assq);
    try vm.builder().defineNativeProc(&assv);
    try vm.builder().defineNativeProc(&assoc);
    try vm.builder().defineNativeProc(&list_copy);
}

/// Native implementation of the `pair?` type predicate.
///
/// Determines whether the given argument is a pair (cons cell) or not.
/// This is the standard Scheme predicate for testing pair types.
///
/// Args:
///   arg: The value to test for being a pair
///
/// Returns:
///   `#t` if the argument is a pair, `#f` otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const pair_predicate = NativeProc.Native{
    .name = "pair?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }
            return Val.init(args[0].isPair());
        }
    }.func,
};

/// Native implementation of the `cons` constructor.
///
/// Creates a new pair (cons cell) with the given car and cdr values.
/// This is the fundamental list constructor in Scheme.
///
/// Args:
///   car: The first element of the pair
///   cdr: The second element of the pair
///
/// Returns:
///   A new pair containing the car and cdr values
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `invalid-argument`: When pair construction fails (allocation error)
const cons = NativeProc.Native{
    .name = "cons",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const pair = Pair.init(args[0], args[1]);
            return ctx.vm.builder().build(pair) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            };
        }
    }.func,
};

/// Native implementation of the `car` accessor.
///
/// Returns the first element (car) of a pair. This is one of the fundamental
/// pair accessors in Scheme, used to extract the head of a list.
///
/// Args:
///   pair: A pair value to extract the car from
///
/// Returns:
///   The car (first element) of the pair
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a pair or pair resolution fails
const car = NativeProc.Native{
    .name = "car",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .pair => |handle| {
                    const pair = ctx.vm.inspector().resolve(Pair, handle) catch {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    };
                    return pair.car;
                },
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            }
        }
    }.func,
};

/// Native implementation of the `cdr` accessor.
///
/// Returns the second element (cdr) of a pair. This is one of the fundamental
/// pair accessors in Scheme, used to extract the tail of a list.
///
/// Args:
///   pair: A pair value to extract the cdr from
///
/// Returns:
///   The cdr (second element) of the pair
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a pair or pair resolution fails
const cdr = NativeProc.Native{
    .name = "cdr",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .pair => |handle| {
                    const pair = ctx.vm.inspector().resolve(Pair, handle) catch {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    };
                    return pair.cdr;
                },
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            }
        }
    }.func,
};

/// Native implementation of the `set-car!` mutator.
///
/// Modifies the first element (car) of a pair in place. This is a destructive
/// operation that changes the pair's internal state.
///
/// Args:
///   pair: A pair value to modify the car of
///   obj: The new value to set as the car
///
/// Returns:
///   The unspecified value (unit value)
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the first argument is not a pair or pair resolution fails
const set_car = NativeProc.Native{
    .name = "set-car!",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .pair => |handle| {
                    const pair_ptr = ctx.vm.pairs.getMutable(handle) orelse {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    };
                    pair_ptr.car = args[1];
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            }
        }
    }.func,
};

/// Native implementation of the `set-cdr!` mutator.
///
/// Modifies the second element (cdr) of a pair in place. This is a destructive
/// operation that changes the pair's internal state.
///
/// Args:
///   pair: A pair value to modify the cdr of
///   obj: The new value to set as the cdr
///
/// Returns:
///   The unspecified value (unit value)
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the first argument is not a pair or pair resolution fails
const set_cdr = NativeProc.Native{
    .name = "set-cdr!",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .pair => |handle| {
                    const pair_ptr = ctx.vm.pairs.getMutable(handle) orelse {
                        try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                        return Val.init({});
                    };
                    pair_ptr.cdr = args[1];
                    return Val.init(ctx.vm.common_symbols.@"*unspecified*");
                },
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            }
        }
    }.func,
};

/// Native implementation of the `null?` type predicate.
///
/// Determines whether the given argument is null (nil/empty list) or not.
/// This is the standard Scheme predicate for testing null values.
///
/// Args:
///   obj: The value to test for being null
///
/// Returns:
///   `#t` if the argument is null, `#f` otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const null_predicate = NativeProc.Native{
    .name = "null?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            switch (args[0].repr) {
                .nil => return Val.init(true),
                else => return Val.init(false),
            }
        }
    }.func,
};

/// Native implementation of the `list?` type predicate.
///
/// Determines whether the given argument is a proper list. A proper list
/// is either null or a pair whose cdr is also a proper list.
///
/// Args:
///   obj: The value to test for being a proper list
///
/// Returns:
///   `#t` if the argument is a proper list, `#f` otherwise
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
const list_predicate = NativeProc.Native{
    .name = "list?",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            return Val.init(isList(ctx.vm, args[0]));
        }

        fn isList(vm: *const Vm, val: Val) bool {
            switch (val.repr) {
                .nil => return true,
                .pair => |handle| {
                    const pair = vm.pairs.get(handle) orelse return false;
                    return isList(vm, pair.cdr);
                },
                else => return false,
            }
        }
    }.func,
};

/// Native implementation of the `make-list` constructor.
///
/// Creates a list of the specified length, optionally filled with a given value.
/// If no fill value is provided, uses the unspecified value.
///
/// Args:
///   k: The length of the list to create (must be a non-negative integer)
///   fill: Optional value to fill the list with (defaults to *unspecified*)
///
/// Returns:
///   A new list of length k filled with the specified value
///
/// Errors:
///   - `wrong-number-of-arguments`: When not 1 or 2 arguments are provided
///   - `type-error`: When k is not an integer
///   - `invalid-argument`: When k is negative or list construction fails
const make_list = NativeProc.Native{
    .name = "make-list",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();

            // Get the fill value (default to *unspecified*)
            const fill = switch (args.len) {
                1 => ctx.vm.common_symbols.@"*unspecified*".toVal(),
                2 => args[1],
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                    return Val.init({});
                },
            };

            // Get the length k
            const k = switch (args[0].repr) {
                .i64 => |n| n,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            };

            // Check that k is non-negative
            if (k < 0) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            }

            // Build the list from the end backwards
            var result = Val.init({}); // Start with nil
            var i: i64 = 0;
            while (i < k) : (i += 1) {
                const pair = Pair.init(fill, result);
                result = ctx.vm.builder().build(pair) catch {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                    return Val.init({});
                };
            }

            return result;
        }
    }.func,
};

/// Native implementation of the `list` constructor.
///
/// Creates a list from the given arguments. This is a convenient way to
/// construct lists without multiple cons operations.
///
/// Args:
///   obj...: Zero or more objects to form into a list
///
/// Returns:
///   A new list containing all the arguments in order
///
/// Errors:
///   - `invalid-argument`: When list construction fails (allocation error)
const list = NativeProc.Native{
    .name = "list",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();

            // Build the list from the end backwards
            var result = Val.init({}); // Start with nil
            var i = args.len;
            while (i > 0) {
                i -= 1;
                const pair = Pair.init(args[i], result);
                result = ctx.vm.builder().build(pair) catch {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                    return Val.init({});
                };
            }

            return result;
        }
    }.func,
};

/// Native implementation of the `length` function.
///
/// Returns the length of a proper list. It is an error to pass an improper list.
///
/// Args:
///   list: A proper list to measure
///
/// Returns:
///   The length of the list as an integer
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a proper list
const length = NativeProc.Native{
    .name = "length",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const len = listLength(ctx.vm, args[0]) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return Val.init(@as(i64, @intCast(len)));
        }

        fn listLength(vm: *const Vm, val: Val) !usize {
            var current = val;
            var count: usize = 0;
            while (true) {
                switch (current.repr) {
                    .nil => return count,
                    .pair => |handle| {
                        const pair = vm.pairs.get(handle) orelse return error.InvalidList;
                        current = pair.cdr;
                        count += 1;
                    },
                    else => return error.InvalidList,
                }
            }
        }
    }.func,
};

/// Native implementation of the `append` function.
///
/// Creates a new list by concatenating the given lists. All but the last argument
/// must be proper lists. The last argument can be any value and becomes the final cdr.
///
/// Args:
///   list...: Zero or more lists to concatenate
///
/// Returns:
///   A new list containing all elements from the input lists
///
/// Errors:
///   - `type-error`: When a non-final argument is not a proper list
///   - `invalid-argument`: When list construction fails (allocation error)
const append = NativeProc.Native{
    .name = "append",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();

            // No arguments returns empty list
            if (args.len == 0) {
                return Val.init({});
            }

            // Single argument returns the argument itself
            if (args.len == 1) {
                return args[0];
            }

            // Multiple arguments: append all but last to the last
            var result = args[args.len - 1];
            var i = args.len - 1;
            while (i > 0) {
                i -= 1;
                result = appendTwo(ctx.vm, args[i], result) catch {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                };
            }

            return result;
        }

        fn appendTwo(vm: *Vm, list1: Val, list2: Val) !Val {
            switch (list1.repr) {
                .nil => return list2,
                .pair => |handle| {
                    const pair = vm.pairs.get(handle) orelse return error.InvalidList;
                    const rest = try appendTwo(vm, pair.cdr, list2);
                    const new_pair = Pair.init(pair.car, rest);
                    return vm.builder().build(new_pair) catch error.AllocationFailed;
                },
                else => return error.InvalidList,
            }
        }
    }.func,
};

/// Native implementation of the `reverse` function.
///
/// Returns a new list with elements in reverse order.
///
/// Args:
///   list: A proper list to reverse
///
/// Returns:
///   A new list with elements in reverse order
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a proper list
///   - `invalid-argument`: When list construction fails (allocation error)
const reverse = NativeProc.Native{
    .name = "reverse",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = reverseList(ctx.vm, args[0]) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return result;
        }

        fn reverseList(vm: *Vm, lst: Val) !Val {
            var result = Val.init({}); // Start with nil
            var current = lst;

            while (true) {
                switch (current.repr) {
                    .nil => return result,
                    .pair => |handle| {
                        const pair = vm.pairs.get(handle) orelse return error.InvalidList;
                        const new_pair = Pair.init(pair.car, result);
                        result = vm.builder().build(new_pair) catch return error.AllocationFailed;
                        current = pair.cdr;
                    },
                    else => return error.InvalidList,
                }
            }
        }
    }.func,
};

/// Native implementation of the `list-tail` function.
///
/// Returns the sublist obtained by omitting the first k elements from the list.
/// It is an error if the list has fewer than k elements.
///
/// Args:
///   list: A proper list
///   k: The number of elements to skip (non-negative integer)
///
/// Returns:
///   The sublist starting from the kth position
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When k is not an integer or list is not proper
///   - `invalid-argument`: When k is negative or k > length of list
const list_tail = NativeProc.Native{
    .name = "list-tail",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            // Get k
            const k = switch (args[1].repr) {
                .i64 => |n| n,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            };

            // Check that k is non-negative
            if (k < 0) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            }

            const result = skipElements(ctx.vm, args[0], @intCast(k)) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            };

            return result;
        }

        fn skipElements(vm: *const Vm, lst: Val, k: usize) !Val {
            var current = lst;
            var remaining = k;

            while (remaining > 0) {
                switch (current.repr) {
                    .nil => return error.IndexOutOfBounds,
                    .pair => |handle| {
                        const pair = vm.pairs.get(handle) orelse return error.InvalidList;
                        current = pair.cdr;
                        remaining -= 1;
                    },
                    else => return error.InvalidList,
                }
            }

            return current;
        }
    }.func,
};

/// Native implementation of the `list-ref` function.
///
/// Returns the kth element of the list (0-indexed).
/// It is an error if the list has k or fewer elements.
///
/// Args:
///   list: A proper list
///   k: The index of the element to retrieve (non-negative integer)
///
/// Returns:
///   The kth element of the list
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When k is not an integer or list is not proper
///   - `invalid-argument`: When k is negative or k >= length of list
const list_ref = NativeProc.Native{
    .name = "list-ref",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            // Get k
            const k = switch (args[1].repr) {
                .i64 => |n| n,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            };

            // Check that k is non-negative
            if (k < 0) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            }

            const result = getElement(ctx.vm, args[0], @intCast(k)) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            };

            return result;
        }

        fn getElement(vm: *const Vm, lst: Val, k: usize) !Val {
            var current = lst;
            var remaining = k;

            while (true) {
                switch (current.repr) {
                    .nil => return error.IndexOutOfBounds,
                    .pair => |handle| {
                        const pair = vm.pairs.get(handle) orelse return error.InvalidList;
                        if (remaining == 0) {
                            return pair.car;
                        }
                        current = pair.cdr;
                        remaining -= 1;
                    },
                    else => return error.InvalidList,
                }
            }
        }
    }.func,
};

/// Native implementation of the `list-set!` function.
///
/// Destructively modifies the kth element of the list.
/// It is an error if the list has k or fewer elements.
///
/// Args:
///   list: A proper list to modify
///   k: The index of the element to set (non-negative integer)
///   obj: The new value to set
///
/// Returns:
///   The unspecified value
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 3 arguments are provided
///   - `type-error`: When k is not an integer or list is not proper
///   - `invalid-argument`: When k is negative or k >= length of list
const list_set = NativeProc.Native{
    .name = "list-set!",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 3) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            // Get k
            const k = switch (args[1].repr) {
                .i64 => |n| n,
                else => {
                    try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                    return Val.init({});
                },
            };

            // Check that k is non-negative
            if (k < 0) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            }

            setElement(ctx.vm, args[0], @intCast(k), args[2]) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"invalid-argument"));
                return Val.init({});
            };

            return Val.init(ctx.vm.common_symbols.@"*unspecified*");
        }

        fn setElement(vm: *Vm, lst: Val, k: usize, new_val: Val) !void {
            var current = lst;
            var remaining = k;

            while (true) {
                switch (current.repr) {
                    .nil => return error.IndexOutOfBounds,
                    .pair => |handle| {
                        if (remaining == 0) {
                            const pair_ptr = vm.pairs.getMutable(handle) orelse return error.InvalidList;
                            pair_ptr.car = new_val;
                            return;
                        }
                        const pair = vm.pairs.get(handle) orelse return error.InvalidList;
                        current = pair.cdr;
                        remaining -= 1;
                    },
                    else => return error.InvalidList,
                }
            }
        }
    }.func,
};

/// Native implementation of the `memq` function.
///
/// Returns the first sublist of list whose car is obj, as determined by eq?.
/// If obj does not occur in list, returns #f.
///
/// Args:
///   obj: The object to search for
///   list: A proper list to search in
///
/// Returns:
///   The sublist starting from the first occurrence of obj, or #f if not found
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the second argument is not a proper list
const memq = NativeProc.Native{
    .name = "memq",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = memSearch(ctx.vm, args[0], args[1], .eq) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return result;
        }
    }.func,
};

/// Native implementation of the `memv` function.
///
/// Returns the first sublist of list whose car is obj, as determined by eqv?.
/// If obj does not occur in list, returns #f.
///
/// Args:
///   obj: The object to search for
///   list: A proper list to search in
///
/// Returns:
///   The sublist starting from the first occurrence of obj, or #f if not found
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the second argument is not a proper list
const memv = NativeProc.Native{
    .name = "memv",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = memSearch(ctx.vm, args[0], args[1], .eqv) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return result;
        }
    }.func,
};

/// Native implementation of the `member` function.
///
/// TODO: Add support for passing in `compare` parameter.
///
/// Returns the first sublist of list whose car is obj, as determined by equal?.
/// If obj does not occur in list, returns #f.
///
/// Args:
///   obj: The object to search for
///   list: A proper list to search in
///
/// Returns:
///   The sublist starting from the first occurrence of obj, or #f if not found
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the second argument is not a proper list
const member = NativeProc.Native{
    .name = "member",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = memSearch(ctx.vm, args[0], args[1], .equal) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };
            return result;
        }
    }.func,
};

/// Comparison types for membership search
const ComparisonType = enum {
    eq,
    eqv,
    equal,
};

/// Search for an object in a list using built-in comparison predicates
fn memSearch(vm: *Vm, obj: Val, lst: Val, comptime comparison: ComparisonType) !Val {
    var current = lst;
    while (true) {
        switch (current.repr) {
            .nil => return Val.init(false),
            .pair => |handle| {
                const pair = vm.pairs.get(handle) orelse return error.InvalidList;

                const matches = switch (comparison) {
                    .eq => obj.eq(pair.car),
                    .eqv => obj.eq(pair.car), // eqv? has same implementation as eq? in this codebase
                    .equal => blk: {
                        break :blk try equivalence.equalRecursive(vm, obj, pair.car);
                    },
                };

                if (matches) {
                    return current;
                }
                current = pair.cdr;
            },
            else => return error.InvalidList,
        }
    }
}

/// Native implementation of the `assq` function.
///
/// Returns the first pair in alist whose car is obj, as determined by eq?.
/// If no such pair exists, returns #f.
///
/// Args:
///   obj: The object to search for as a key
///   alist: An association list (list of pairs)
///
/// Returns:
///   The first pair with matching car, or #f if not found
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the second argument is not a proper list
const assq = NativeProc.Native{
    .name = "assq",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = assocSearch(ctx.vm, args[0], args[1], .eq) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return result;
        }
    }.func,
};

/// Native implementation of the `assv` function.
///
/// Returns the first pair in alist whose car is obj, as determined by eqv?.
/// If no such pair exists, returns #f.
///
/// Args:
///   obj: The object to search for as a key
///   alist: An association list (list of pairs)
///
/// Returns:
///   The first pair with matching car, or #f if not found
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the second argument is not a proper list
const assv = NativeProc.Native{
    .name = "assv",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = assocSearch(ctx.vm, args[0], args[1], .eqv) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return result;
        }
    }.func,
};

/// Native implementation of the `assoc` function.
///
/// TODO: Add support for passing `compare` parameter.
///
/// Returns the first pair in alist whose car is obj, as determined by equal?.
/// If no such pair exists, returns #f.
///
/// Args:
///   obj: The object to search for as a key
///   alist: An association list (list of pairs)
///
/// Returns:
///   The first pair with matching car, or #f if not found
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 2 arguments are provided
///   - `type-error`: When the second argument is not a proper list
const assoc = NativeProc.Native{
    .name = "assoc",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 2) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = assocSearch(ctx.vm, args[0], args[1], .equal) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return result;
        }
    }.func,
};

/// Search for a key in an association list using the specified comparison predicate
fn assocSearch(vm: *Vm, obj: Val, alist: Val, comptime comparison: ComparisonType) !Val {
    var current = alist;
    while (true) {
        switch (current.repr) {
            .nil => return Val.init(false),
            .pair => |handle| {
                const pair = vm.pairs.get(handle) orelse return error.InvalidList;

                // Each element of the alist should be a pair
                switch (pair.car.repr) {
                    .pair => |key_pair_handle| {
                        const key_pair = vm.pairs.get(key_pair_handle) orelse return error.InvalidList;

                        const matches = switch (comparison) {
                            .eq => obj.eq(key_pair.car),
                            .eqv => obj.eq(key_pair.car), // eqv? has same implementation as eq? in this codebase
                            .equal => blk: {
                                break :blk try equivalence.equalRecursive(vm, obj, key_pair.car);
                            },
                        };

                        if (matches) {
                            return pair.car; // Return the matching pair
                        }
                    },
                    else => {
                        // Non-pair elements in alist are ignored (standard behavior)
                    },
                }
                current = pair.cdr;
            },
            else => return error.InvalidList,
        }
    }
}

/// Native implementation of the `list-copy` function.
///
/// Returns a shallow copy of the list. The pairs are newly allocated but
/// the cars of the pairs are shared (copied by reference).
///
/// Args:
///   list: A proper list to copy
///
/// Returns:
///   A new list with the same elements
///
/// Errors:
///   - `wrong-number-of-arguments`: When not exactly 1 argument is provided
///   - `type-error`: When the argument is not a proper list
///   - `invalid-argument`: When list construction fails (allocation error)
const list_copy = NativeProc.Native{
    .name = "list-copy",
    .func = struct {
        fn func(ctx: NativeProc.NativeContext) Vm.Error!Val {
            const args = ctx.localStack();
            if (args.len != 1) {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"wrong-number-of-arguments"));
                return Val.init({});
            }

            const result = copyList(ctx.vm, args[0]) catch {
                try instruction.raiseWithError(ctx.vm, Val.init(ctx.vm.common_symbols.@"type-error"));
                return Val.init({});
            };

            return result;
        }

        fn copyList(vm: *Vm, lst: Val) !Val {
            switch (lst.repr) {
                .nil => return Val.init({}),
                .pair => |handle| {
                    const pair = vm.pairs.get(handle) orelse return error.InvalidList;
                    const copied_cdr = try copyList(vm, pair.cdr);
                    const new_pair = Pair.init(pair.car, copied_cdr);
                    return vm.builder().build(new_pair) catch error.AllocationFailed;
                },
                else => return error.InvalidList,
            }
        }
    }.func,
};

////////////////////////////////////////////////////////////////////////////////
// Pair predicate tests
////////////////////////////////////////////////////////////////////////////////

test "pair? with pair returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(pair? (cons 1 2))");
}

test "pair? with list returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(pair? '(1 2 3))");
}

test "pair? with nil returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(pair? '())");
}

test "pair? with integer returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(pair? 42)");
}

test "pair? with symbol returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(pair? 'hello)");
}

test "pair? with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(pair?)"),
    );
}

test "pair? with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(pair? (cons 1 2) (cons 3 4))"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Cons tests
////////////////////////////////////////////////////////////////////////////////

test "cons creates pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 . 2)", "(cons 1 2)");
}

test "cons with nil creates list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1)", "(cons 1 '())");
}

test "cons with list extends list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(0 1 2)", "(cons 0 '(1 2))");
}

test "cons with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cons)"),
    );
}

test "cons with one argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cons 1)"),
    );
}

test "cons with three arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cons 1 2 3)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Car tests
////////////////////////////////////////////////////////////////////////////////

test "car returns first element of pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(car (cons 1 2))");
}

test "car returns first element of list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(car '(1 2 3))");
}

test "car with nil is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(car '())"),
    );
}

test "car with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(car 42)"),
    );
}

test "car with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(car)"),
    );
}

test "car with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(car (cons 1 2) (cons 3 4))"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// Cdr tests
////////////////////////////////////////////////////////////////////////////////

test "cdr returns second element of pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(cdr (cons 1 2))");
}

test "cdr returns rest of list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(2 3)", "(cdr '(1 2 3))");
}

test "cdr of single element list returns nil" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(cdr '(1))");
}

test "cdr with nil is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr '())"),
    );
}

test "cdr with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr 42)"),
    );
}

test "cdr with no arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr)"),
    );
}

test "cdr with multiple arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(cdr (cons 1 2) (cons 3 4))"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// set-car! tests
////////////////////////////////////////////////////////////////////////////////

test "set-car! modifies first element of pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define p (cons 1 2))");
    try vm.expectEval("*unspecified*", "(set-car! p 42)");
    try vm.expectEval("(42 . 2)", "p");
}

test "set-car! with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-car! 42 100)"),
    );
}

test "set-car! with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-car!)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-car! (cons 1 2))"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-car! (cons 1 2) 3 4)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// set-cdr! tests
////////////////////////////////////////////////////////////////////////////////

test "set-cdr! modifies second element of pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define p (cons 1 2))");
    try vm.expectEval("*unspecified*", "(set-cdr! p 42)");
    try vm.expectEval("1", "(car p)");
    try vm.expectEval("42", "(cdr p)");
}

test "set-cdr! with non-pair is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-cdr! 42 100)"),
    );
}

test "set-cdr! with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-cdr!)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-cdr! (cons 1 2))"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(set-cdr! (cons 1 2) 3 4)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// null? tests
////////////////////////////////////////////////////////////////////////////////

test "null? with nil returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(null? '())");
}

test "null? with pair returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(null? (cons 1 2))");
}

test "null? with other types returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(null? 42)");
    try vm.expectEval("#f", "(null? #t)");
    try vm.expectEval("#f", "(null? 'symbol)");
}

test "null? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(null?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(null? '() '())"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// list? tests
////////////////////////////////////////////////////////////////////////////////

test "list? with nil returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(list? '())");
}

test "list? with proper list returns #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(list? '(1 2 3))");
    try vm.expectEval("#t", "(list? (cons 1 '()))");
    try vm.expectEval("#t", "(list? (cons 1 (cons 2 '())))");
}

test "list? with improper list returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(list? (cons 1 2))");
}

test "list? with non-list returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(list? 42)");
    try vm.expectEval("#f", "(list? #t)");
    try vm.expectEval("#f", "(list? 'symbol)");
}

test "list? with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list?)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list? '() '())"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// make-list tests
////////////////////////////////////////////////////////////////////////////////

test "make-list with zero length returns empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(make-list 0)");
    try vm.expectEval("()", "(make-list 0 42)");
}

test "make-list with positive length and no fill uses *unspecified*" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(*unspecified*)", "(make-list 1)");
    try vm.expectEval("(*unspecified* *unspecified*)", "(make-list 2)");
    try vm.expectEval("(*unspecified* *unspecified* *unspecified*)", "(make-list 3)");
}

test "make-list with positive length and fill value" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42)", "(make-list 1 42)");
    try vm.expectEval("(#t #t)", "(make-list 2 #t)");
    try vm.expectEval("(hello hello hello)", "(make-list 3 'hello)");
}

test "make-list creates proper lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(list? (make-list 0))");
    try vm.expectEval("#t", "(list? (make-list 1))");
    try vm.expectEval("#t", "(list? (make-list 5 'x))");
}

test "make-list with negative length is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(make-list -1)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(make-list -5 42)"),
    );
}

test "make-list with non-integer length is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(make-list #t)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(make-list 'hello)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(make-list 3.14)"),
    );
}

test "make-list with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(make-list)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(make-list 1 2 3)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// list tests
////////////////////////////////////////////////////////////////////////////////

test "list with no arguments returns empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(list)");
}

test "list with single argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42)", "(list 42)");
    try vm.expectEval("(hello)", "(list 'hello)");
    try vm.expectEval("(#t)", "(list #t)");
}

test "list with multiple arguments" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 3)", "(list 1 2 3)");
    try vm.expectEval("(hello world)", "(list 'hello 'world)");
    try vm.expectEval("(#t #f 42)", "(list #t #f 42)");
}

test "list creates proper lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(list? (list))");
    try vm.expectEval("#t", "(list? (list 1))");
    try vm.expectEval("#t", "(list? (list 1 2 3))");
}

test "list elements can be accessed with car/cdr" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(car (list 1 2 3))");
    try vm.expectEval("(2 3)", "(cdr (list 1 2 3))");
    try vm.expectEval("2", "(car (cdr (list 1 2 3)))");
}

test "list is equivalent to nested cons" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? (list) '())");
    try vm.expectEval("#t", "(equal? (list 1) (cons 1 '()))");
    try vm.expectEval("#t", "(equal? (list 1 2) (cons 1 (cons 2 '())))");
    try vm.expectEval("#t", "(equal? (list 1 2 3) (cons 1 (cons 2 (cons 3 '()))))");
}

////////////////////////////////////////////////////////////////////////////////
// length tests
////////////////////////////////////////////////////////////////////////////////

test "length of empty list is 0" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(length '())");
}

test "length of single element list is 1" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(length '(42))");
    try vm.expectEval("1", "(length (list 'hello))");
}

test "length of multi-element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3", "(length '(1 2 3))");
    try vm.expectEval("5", "(length (list 'a 'b 'c 'd 'e))");
}

test "length with improper list is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(length (cons 1 2))"),
    );
}

test "length with non-list is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(length 42)"),
    );
}

test "length with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(length)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(length '() '())"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// append tests
////////////////////////////////////////////////////////////////////////////////

test "append with no arguments returns empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(append)");
}

test "append with single argument returns argument" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 3)", "(append '(1 2 3))");
    try vm.expectEval("42", "(append 42)");
}

test "append two empty lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(append '() '())");
}

test "append empty list and non-empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 3)", "(append '() '(1 2 3))");
    try vm.expectEval("(1 2 3)", "(append '(1 2 3) '())");
}

test "append two non-empty lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 3 4)", "(append '(1 2) '(3 4))");
    try vm.expectEval("(a b c d e f)", "(append '(a b c) '(d e f))");
}

test "append multiple lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 3 4 5 6)", "(append '(1 2) '(3 4) '(5 6))");
    try vm.expectEval("(a b c d)", "(append '(a) '(b) '(c) '(d))");
}

test "append with improper final argument creates improper list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 . 3)", "(append '(1 2) 3)");
    try vm.expectEval("(a b c . d)", "(append '(a b) '(c) 'd)");
}

test "append with improper non-final argument is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(append (cons 1 2) '(3 4))"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// reverse tests
////////////////////////////////////////////////////////////////////////////////

test "reverse empty list returns empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(reverse '())");
}

test "reverse single element list returns same list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42)", "(reverse '(42))");
}

test "reverse multi-element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(3 2 1)", "(reverse '(1 2 3))");
    try vm.expectEval("(e d c b a)", "(reverse '(a b c d e))");
}

test "reverse creates proper lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(list? (reverse '()))");
    try vm.expectEval("#t", "(list? (reverse '(1 2 3)))");
}

test "reverse with improper list is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(reverse (cons 1 2))"),
    );
}

test "reverse with non-list is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(reverse 42)"),
    );
}

test "reverse with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(reverse)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(reverse '() '())"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// list-tail tests
////////////////////////////////////////////////////////////////////////////////

test "list-tail with k=0 returns original list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(list-tail '() 0)");
    try vm.expectEval("(1 2 3)", "(list-tail '(1 2 3) 0)");
}

test "list-tail with k=1 returns cdr" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(2 3)", "(list-tail '(1 2 3) 1)");
    try vm.expectEval("()", "(list-tail '(1) 1)");
}

test "list-tail with k=length returns empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(list-tail '(1 2 3) 3)");
}

test "list-tail with k > length is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail '(1 2 3) 4)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail '() 1)"),
    );
}

test "list-tail with negative k is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail '(1 2 3) -1)"),
    );
}

test "list-tail with non-integer k is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail '(1 2 3) #t)"),
    );
}

test "list-tail with improper list can access existing elements" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // (cons 1 2) creates (1 . 2), skipping 1 element gives us 2
    try vm.expectEval("2", "(list-tail (cons 1 2) 1)");

    // But trying to skip 2 elements should be an error
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail (cons 1 2) 2)"),
    );
}

test "list-tail with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail '(1 2 3))"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-tail '(1 2 3) 1 2)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// list-ref tests
////////////////////////////////////////////////////////////////////////////////

test "list-ref returns correct elements" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("1", "(list-ref '(1 2 3) 0)");
    try vm.expectEval("2", "(list-ref '(1 2 3) 1)");
    try vm.expectEval("3", "(list-ref '(1 2 3) 2)");
}

test "list-ref with single element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("42", "(list-ref '(42) 0)");
}

test "list-ref with k >= length is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref '(1 2 3) 3)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref '() 0)"),
    );
}

test "list-ref with negative k is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref '(1 2 3) -1)"),
    );
}

test "list-ref with non-integer k is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref '(1 2 3) #t)"),
    );
}

test "list-ref with improper list is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref (cons 1 2) 1)"),
    );
}

test "list-ref with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref '(1 2 3))"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-ref '(1 2 3) 1 2)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// list-set! tests
////////////////////////////////////////////////////////////////////////////////

test "list-set! modifies elements correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define lst (list 1 2 3))");
    try vm.expectEval("*unspecified*", "(list-set! lst 0 'a)");
    try vm.expectEval("(a 2 3)", "lst");

    try vm.expectEval("*unspecified*", "(list-set! lst 1 'b)");
    try vm.expectEval("(a b 3)", "lst");

    try vm.expectEval("*unspecified*", "(list-set! lst 2 'c)");
    try vm.expectEval("(a b c)", "lst");
}

test "list-set! with single element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define lst (list 42))");
    try vm.expectEval("*unspecified*", "(list-set! lst 0 100)");
    try vm.expectEval("(100)", "lst");
}

test "list-set! with k >= length is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define lst (list 1 2 3))");
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! lst 3 'x)"),
    );

    try vm.expectEval("*unspecified*", "(define empty '())");
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! empty 0 'x)"),
    );
}

test "list-set! with negative k is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define lst (list 1 2 3))");
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! lst -1 'x)"),
    );
}

test "list-set! with non-integer k is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define lst (list 1 2 3))");
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! lst #t 'x)"),
    );
}

test "list-set! with improper list can modify existing elements" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define lst (cons 1 2))");

    // Can modify the first element
    try vm.expectEval("*unspecified*", "(list-set! lst 0 'x)");
    try vm.expectEval("(x . 2)", "lst");

    // But trying to modify beyond what exists should be an error
    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! lst 1 'y)"),
    );
}

test "list-set! with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set!)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! '(1 2 3))"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! '(1 2 3) 0)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-set! '(1 2 3) 0 'x 'y)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// memq tests
////////////////////////////////////////////////////////////////////////////////

test "memq finds element with eq? comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a b c)", "(memq 'a '(x y a b c))");
    try vm.expectEval("(b c)", "(memq 'b '(x y a b c))");
    try vm.expectEval("(c)", "(memq 'c '(x y a b c))");
}

test "memq returns #f when element not found" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(memq 'z '(a b c))");
    try vm.expectEval("#f", "(memq 42 '(1 2 3))");
}

test "memq with empty list returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(memq 'a '())");
}

test "memq with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(memq)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(memq 'a)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(memq 'a '(1 2 3) 'extra)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// memv tests
////////////////////////////////////////////////////////////////////////////////

test "memv finds element with eqv? comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42 100)", "(memv 42 '(1 2 42 100))");
    try vm.expectEval("(#t #f)", "(memv #t '(1 #f #t #f))");
}

test "memv returns #f when element not found" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(memv 99 '(1 2 3))");
    try vm.expectEval("#f", "(memv 'z '(a b c))");
}

test "memv with empty list returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(memv 42 '())");
}

test "memv with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(memv)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(memv 42)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(memv 42 '(1 2 3) 'extra)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// member tests
////////////////////////////////////////////////////////////////////////////////

test "member finds element with equal? comparison by default" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42 100)", "(member 42 '(1 2 42 100))");
    try vm.expectEval("((a b) c)", "(member '(a b) '(x y (a b) c))");
}

test "member returns #f when element not found" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(member 99 '(1 2 3))");
    try vm.expectEval("#f", "(member '(x y) '((a b) (c d)))");
}

test "member with empty list returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(member 42 '())");
}

test "member with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(member)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(member 42)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(member 42 '(1 2 3) 'extra)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// assq tests
////////////////////////////////////////////////////////////////////////////////

test "assq finds pair with eq? comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a . 1)", "(assq 'a '((a . 1) (b . 2) (c . 3)))");
    try vm.expectEval("(b . 2)", "(assq 'b '((a . 1) (b . 2) (c . 3)))");
    try vm.expectEval("(c . 3)", "(assq 'c '((a . 1) (b . 2) (c . 3)))");
}

test "assq returns #f when key not found" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(assq 'd '((a . 1) (b . 2) (c . 3)))");
    try vm.expectEval("#f", "(assq 1 '((a . 1) (b . 2)))");
}

test "assq with empty alist returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(assq 'a '())");
}

test "assq returns first matching pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a . 1)", "(assq 'a '((a . 1) (b . 2) (a . 3)))");
}

test "assq ignores non-pair elements in alist" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a . 1)", "(assq 'a '(not-a-pair (a . 1) (b . 2)))");
}

test "assq with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assq)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assq 'a)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assq 'a '((a . 1)) 'extra)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// assv tests
////////////////////////////////////////////////////////////////////////////////

test "assv finds pair with eqv? comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42 . hello)", "(assv 42 '((1 . a) (42 . hello) (3 . c)))");
    try vm.expectEval("(#t . true)", "(assv #t '((#f . false) (#t . true)))");
}

test "assv returns #f when key not found" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(assv 99 '((1 . a) (2 . b)))");
    try vm.expectEval("#f", "(assv 'missing '((1 . a) (2 . b)))");
}

test "assv with empty alist returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(assv 42 '())");
}

test "assv with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assv)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assv 42)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assv 42 '((42 . answer)) 'extra)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// assoc tests
////////////////////////////////////////////////////////////////////////////////

test "assoc finds pair with equal? comparison" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42 . answer)", "(assoc 42 '((1 . a) (42 . answer) (3 . c)))");
    try vm.expectEval("((a b) . found)", "(assoc '(a b) '(((x y) . no) ((a b) . found)))");
}

test "assoc returns #f when key not found" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(assoc 99 '((1 . a) (2 . b)))");
    try vm.expectEval("#f", "(assoc '(x y) '(((a b) . one) ((c d) . two)))");
}

test "assoc with empty alist returns #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(assoc 'key '())");
}

test "assoc uses deep comparison for structured keys" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("((hello world) . greeting)", "(assoc '(hello world) '(((foo bar) . test) ((hello world) . greeting)))");
}

test "assoc with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assoc)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assoc 'key)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(assoc 'key '((key . value)) 'extra)"),
    );
}

////////////////////////////////////////////////////////////////////////////////
// list-copy tests
////////////////////////////////////////////////////////////////////////////////

test "list-copy creates shallow copy of empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("()", "(list-copy '())");
}

test "list-copy creates shallow copy of single element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(42)", "(list-copy '(42))");
    try vm.expectEval("(hello)", "(list-copy '(hello))");
}

test "list-copy creates shallow copy of multi-element list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 3)", "(list-copy '(1 2 3))");
    try vm.expectEval("(a b c d)", "(list-copy '(a b c d))");
}

test "list-copy preserves nested structure" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("((1 2) (3 4))", "(list-copy '((1 2) (3 4)))");
    try vm.expectEval("(1 (2 (3 4)))", "(list-copy '(1 (2 (3 4))))");
}

test "list-copy creates independent copy" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("*unspecified*", "(define original '(1 2 3))");
    try vm.expectEval("*unspecified*", "(define copied (list-copy original))");
    try vm.expectEval("(1 2 3)", "copied");

    // Verify they are equal but not the same object
    try vm.expectEval("#t", "(equal? original copied)");
    // Note: We can't easily test that they're not eq? without modifying one
}

test "list-copy with wrong number of arguments is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-copy)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-copy '(1 2 3) 'extra)"),
    );
}

test "list-copy with non-list is error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-copy 42)"),
    );

    try testing.expectError(
        Vm.Error.UncaughtException,
        vm.evalStr("(list-copy 'not-a-list)"),
    );
}
