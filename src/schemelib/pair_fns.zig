const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Pair = @import("../types/Pair.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const car = NativeProc.with1Arg(struct {
    pub const name = "car";
    pub const docstring =
        \\(car pair)
        \\
        \\Returns the first element of a pair.
        \\(car '(a . b))  =>  a
        \\(car '(a b c))  =>  a
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const pair = inspector.asPair(arg) catch {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "pair",
                    .got = arg,
                    .proc = Val.initNativeProc(&car),
                    .arg_name = "pair",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        return pair.car;
    }
});

pub const cdr = NativeProc.with1Arg(struct {
    pub const name = "cdr";
    pub const docstring =
        \\(cdr pair)
        \\
        \\Returns the second element of a pair (rest of the list).
        \\(cdr '(a . b))  =>  b
        \\(cdr '(a b c))  =>  (b c)
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const pair = inspector.asPair(arg) catch {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "pair",
                    .got = arg,
                    .proc = Val.initNativeProc(&cdr),
                    .arg_name = "pair",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        return pair.cdr;
    }
});

pub const cons = NativeProc.with2Args(struct {
    pub const name = "cons";
    pub const docstring =
        \\(cons obj1 obj2)
        \\
        \\Constructs a new pair from two values.
        \\(cons 'a 'b)      =>  (a . b)
        \\(cons 'a '(b c))  =>  (a b c)
    ;
    pub inline fn impl(vm: *Vm, _: ?*Diagnostics, arg1: Val, arg2: Val) Vm.Error!Val {
        const builder = vm.builder();
        const pair = builder.makePair(arg1, arg2) catch return error.OutOfMemory;
        return pair;
    }
});

pub const pair_p = NativeProc.with1Arg(struct {
    pub const name = "pair?";
    pub const docstring =
        \\(pair? obj)
        \\
        \\Returns #t if object is a pair, #f otherwise.
        \\(pair? '(a . b))  =>  #t
        \\(pair? '())       =>  #f
    ;
    pub inline fn impl(_: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const is_pair = switch (arg.data) {
            .pair => true,
            else => false,
        };
        return Val.initBool(is_pair);
    }
});

pub const null_p = NativeProc.with1Arg(struct {
    pub const name = "null?";
    pub const docstring =
        \\(null? obj)
        \\
        \\Returns #t if object is the empty list, #f otherwise.
        \\(null? '())       =>  #t
        \\(null? '(a b c))  =>  #f
    ;
    pub inline fn impl(_: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const is_null = switch (arg.data) {
            .empty_list => true,
            else => false,
        };
        return Val.initBool(is_null);
    }
});

pub const set_car_b = NativeProc.with2Args(struct {
    pub const name = "set-car!";
    pub const docstring =
        \\(set-car! pair obj)
        \\
        \\Mutates the car field of a pair.
        \\Returns an unspecified value.
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, pair_val: Val, new_car: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const pair = inspector.asPair(pair_val) catch {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "pair",
                    .got = pair_val,
                    .proc = Val.initNativeProc(&set_car_b),
                    .arg_name = "pair",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        pair.car = new_car;
        return Val.initUnspecified();
    }
});

pub const set_cdr_b = NativeProc.with2Args(struct {
    pub const name = "set-cdr!";
    pub const docstring =
        \\(set-cdr! pair obj)
        \\
        \\Mutates the cdr field of a pair.
        \\Returns an unspecified value.
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, pair_val: Val, new_cdr: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const pair = inspector.asPair(pair_val) catch {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "pair",
                    .got = pair_val,
                    .proc = Val.initNativeProc(&set_cdr_b),
                    .arg_name = "pair",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        pair.cdr = new_cdr;
        return Val.initUnspecified();
    }
});

pub const list_p = NativeProc.with1Arg(struct {
    pub const name = "list?";
    pub const docstring =
        \\(list? obj)
        \\
        \\Returns #t if obj is a proper list, #f otherwise.
        \\(list? '(a b c))  =>  #t
        \\(list? '())       =>  #t
        \\(list? '(a . b))  =>  #f
    ;
    pub inline fn impl(vm: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();

        var current = arg;
        while (true) {
            switch (current.data) {
                .empty_list => return Val.initBool(true),
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return error.UndefinedBehavior;
                    current = pair.cdr;
                },
                else => return Val.initBool(false),
            }
        }
    }
});

pub const make_list = NativeProc.withRawArgs(struct {
    pub const name = "make-list";
    pub const docstring =
        \\(make-list k)
        \\(make-list k fill)
        \\
        \\Creates a list of k elements.
        \\With one argument, returns a list of k unspecified values.
        \\With two arguments, returns a list of k copies of fill.
        \\(make-list 3)       =>  list of 3 unspecified values
        \\(make-list 3 'x)    =>  (x x x)
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        if (args.len < 1 or args.len > 2) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_count = .{
                    .expected = 1,
                    .got = @intCast(args.len),
                    .proc = Val.initNativeProc(&make_list),
                } });
            }
            return Vm.Error.UncaughtException;
        }

        const k = args[0].asInt() orelse {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "integer",
                    .got = args[0],
                    .proc = Val.initNativeProc(&make_list),
                    .arg_name = "k",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        if (k < 0) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .other = "make-list: k must be non-negative" });
            }
            return Vm.Error.UncaughtException;
        }

        const fill = if (args.len == 2) args[1] else Val.initUnspecified();
        const builder = vm.builder();

        var result = Val.initEmptyList();
        var i: i64 = 0;
        while (i < k) : (i += 1) {
            result = builder.makePair(fill, result) catch return error.OutOfMemory;
        }

        return result;
    }
});

pub const list = NativeProc.withRawArgs(struct {
    pub const name = "list";
    pub const docstring =
        \\(list obj ...)
        \\
        \\Returns a list of its arguments.
        \\(list 'a 'b 'c)  =>  (a b c)
        \\(list)           =>  ()
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        _ = diagnostics;
        const builder = vm.builder();
        const result = builder.makeList(args) catch return error.OutOfMemory;
        return result;
    }
});

pub const length = NativeProc.with1Arg(struct {
    pub const name = "length";
    pub const docstring =
        \\(length list)
        \\
        \\Returns the length of a list.
        \\(length '(a b c))  =>  3
        \\(length '())       =>  0
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();

        var len: i64 = 0;
        var current = arg;
        while (true) {
            switch (current.data) {
                .empty_list => return Val.initInt(len),
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return error.UndefinedBehavior;
                    len += 1;
                    current = pair.cdr;
                },
                else => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .wrong_arg_type = .{
                            .expected = "list",
                            .got = arg,
                            .proc = Val.initNativeProc(&length),
                            .arg_name = "list",
                            .arg_position = 0,
                        } });
                    }
                    return Vm.Error.UncaughtException;
                },
            }
        }
    }
});

pub const append = NativeProc.withRawArgs(struct {
    pub const name = "append";
    pub const docstring =
        \\(append list ...)
        \\
        \\Concatenates lists together.
        \\The last argument need not be a list (improper append).
        \\(append '(a b) '(c d))  =>  (a b c d)
        \\(append)                =>  ()
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        if (args.len == 0) return Val.initEmptyList();
        if (args.len == 1) return args[0];

        const builder = vm.builder();
        const inspector = vm.inspector();

        // The last argument is not copied, just becomes the tail
        const last_arg = args[args.len - 1];

        // Build result from right to left, processing each list except the last
        var result = last_arg;
        var list_idx = args.len - 1;
        while (list_idx > 0) {
            list_idx -= 1;
            const current_list = args[list_idx];

            // Convert the current list to a slice for easy processing
            var items = std.ArrayList(Val){};
            defer items.deinit(vm.allocator());

            var current = current_list;
            while (true) {
                switch (current.data) {
                    .empty_list => break,
                    .pair => |h| {
                        const pair = inspector.handleToPair(h) catch return error.UndefinedBehavior;
                        items.append(vm.allocator(), pair.car) catch return error.OutOfMemory;
                        current = pair.cdr;
                    },
                    else => {
                        if (diagnostics) |d| {
                            d.addDiagnostic(.{ .wrong_arg_type = .{
                                .expected = "list",
                                .got = current_list,
                                .proc = Val.initNativeProc(&append),
                                .arg_name = null,
                                .arg_position = @intCast(list_idx),
                            } });
                        }
                        return Vm.Error.UncaughtException;
                    },
                }
            }

            // Build this list in reverse onto the result
            const items_slice = items.items;
            var i = items_slice.len;
            while (i > 0) {
                i -= 1;
                result = builder.makePair(items_slice[i], result) catch return error.OutOfMemory;
            }
        }

        return result;
    }
});

pub const reverse = NativeProc.with1Arg(struct {
    pub const name = "reverse";
    pub const docstring =
        \\(reverse list)
        \\
        \\Returns a newly allocated list with elements in reverse order.
        \\(reverse '(a b c))  =>  (c b a)
        \\(reverse '())       =>  ()
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const builder = vm.builder();

        var result = Val.initEmptyList();
        var current = arg;
        while (true) {
            switch (current.data) {
                .empty_list => return result,
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return error.UndefinedBehavior;
                    result = builder.makePair(pair.car, result) catch return error.OutOfMemory;
                    current = pair.cdr;
                },
                else => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .wrong_arg_type = .{
                            .expected = "list",
                            .got = arg,
                            .proc = Val.initNativeProc(&reverse),
                            .arg_name = "list",
                            .arg_position = 0,
                        } });
                    }
                    return Vm.Error.UncaughtException;
                },
            }
        }
    }
});

pub const list_tail = NativeProc.withRawArgs(struct {
    pub const name = "list-tail";
    pub const docstring =
        \\(list-tail list k)
        \\
        \\Returns the sublist obtained by omitting the first k elements.
        \\(list-tail '(a b c d) 2)  =>  (c d)
        \\(list-tail list 0)        =>  list
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        if (args.len != 2) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_count = .{
                    .expected = 2,
                    .got = @intCast(args.len),
                    .proc = Val.initNativeProc(&list_tail),
                } });
            }
            return Vm.Error.UncaughtException;
        }

        const k = args[1].asInt() orelse {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "integer",
                    .got = args[1],
                    .proc = Val.initNativeProc(&list_tail),
                    .arg_name = "k",
                    .arg_position = 1,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        if (k < 0) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .other = "list-tail: k must be non-negative" });
            }
            return Vm.Error.UncaughtException;
        }

        const inspector = vm.inspector();
        var current = args[0];
        var i: i64 = 0;
        while (i < k) : (i += 1) {
            switch (current.data) {
                .empty_list => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .other = "list-tail: list is too short" });
                    }
                    return Vm.Error.UncaughtException;
                },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return error.UndefinedBehavior;
                    current = pair.cdr;
                },
                else => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .wrong_arg_type = .{
                            .expected = "list",
                            .got = args[0],
                            .proc = Val.initNativeProc(&list_tail),
                            .arg_name = "list",
                            .arg_position = 0,
                        } });
                    }
                    return Vm.Error.UncaughtException;
                },
            }
        }

        return current;
    }
});

pub const list_ref = NativeProc.withRawArgs(struct {
    pub const name = "list-ref";
    pub const docstring =
        \\(list-ref list k)
        \\
        \\Returns the kth element of list (0-indexed).
        \\(list-ref '(a b c d) 2)  =>  c
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        if (args.len != 2) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_count = .{
                    .expected = 2,
                    .got = @intCast(args.len),
                    .proc = Val.initNativeProc(&list_ref),
                } });
            }
            return Vm.Error.UncaughtException;
        }

        const k = args[1].asInt() orelse {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "integer",
                    .got = args[1],
                    .proc = Val.initNativeProc(&list_ref),
                    .arg_name = "k",
                    .arg_position = 1,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        if (k < 0) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .other = "list-ref: k must be non-negative" });
            }
            return Vm.Error.UncaughtException;
        }

        const inspector = vm.inspector();
        var current = args[0];
        var i: i64 = 0;
        while (true) {
            switch (current.data) {
                .empty_list => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .other = "list-ref: index out of bounds" });
                    }
                    return Vm.Error.UncaughtException;
                },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return error.UndefinedBehavior;
                    if (i == k) return pair.car;
                    i += 1;
                    current = pair.cdr;
                },
                else => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .wrong_arg_type = .{
                            .expected = "list",
                            .got = args[0],
                            .proc = Val.initNativeProc(&list_ref),
                            .arg_name = "list",
                            .arg_position = 0,
                        } });
                    }
                    return Vm.Error.UncaughtException;
                },
            }
        }
    }
});

pub const list_set_b = NativeProc.withRawArgs(struct {
    pub const name = "list-set!";
    pub const docstring =
        \\(list-set! list k obj)
        \\
        \\Mutates the kth element of list to obj.
        \\Returns an unspecified value.
        \\(list-set! '(a b c d) 2 'x)  mutates list to (a b x d)
    ;
    pub inline fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        if (args.len != 3) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_count = .{
                    .expected = 3,
                    .got = @intCast(args.len),
                    .proc = Val.initNativeProc(&list_set_b),
                } });
            }
            return Vm.Error.UncaughtException;
        }

        const k = args[1].asInt() orelse {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "integer",
                    .got = args[1],
                    .proc = Val.initNativeProc(&list_set_b),
                    .arg_name = "k",
                    .arg_position = 1,
                } });
            }
            return Vm.Error.UncaughtException;
        };
        if (k < 0) {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .other = "list-set!: k must be non-negative" });
            }
            return Vm.Error.UncaughtException;
        }
        const new_val = args[2];

        const inspector = vm.inspector();
        var current = args[0];
        var i: i64 = 0;
        while (true) {
            switch (current.data) {
                .empty_list => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .other = "list-set!: index out of bounds" });
                    }
                    return Vm.Error.UncaughtException;
                },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return error.UndefinedBehavior;
                    if (i == k) {
                        pair.car = new_val;
                        return Val.initUnspecified();
                    }
                    i += 1;
                    current = pair.cdr;
                },
                else => {
                    if (diagnostics) |d| {
                        d.addDiagnostic(.{ .wrong_arg_type = .{
                            .expected = "list",
                            .got = args[0],
                            .proc = Val.initNativeProc(&list_set_b),
                            .arg_name = "list",
                            .arg_position = 0,
                        } });
                    }
                    return Vm.Error.UncaughtException;
                },
            }
        }
    }
});

// Unit Tests

test "car - basic pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("a", "(car '(a . b))");
    try vm.expectEval("1", "(car '(1 2 3))");
}

test "cdr - basic pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("b", "(cdr '(a . b))");
    try vm.expectEval("(2 3)", "(cdr '(1 2 3))");
}

test "cons - construct pair" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a . b)", "(cons 'a 'b)");
    try vm.expectEval("(1 2 3)", "(cons 1 '(2 3))");
    try vm.expectEval("(a)", "(cons 'a '())");
}

test "pair? - predicate" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(pair? '(a . b))");
    try vm.expectEval("#t", "(pair? '(a b c))");
    try vm.expectEval("#f", "(pair? '())");
    try vm.expectEval("#f", "(pair? 'a)");
    try vm.expectEval("#f", "(pair? 42)");
}

test "null? - predicate" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(null? '())");
    try vm.expectEval("#f", "(null? '(a b c))");
    try vm.expectEval("#f", "(null? 'a)");
    try vm.expectEval("#f", "(null? 42)");
}

test "set-car! - mutation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("x", "(define p '(a b c)) (set-car! p 'x) (car p)");
}

test "set-cdr! - mutation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(x y)", "(define p '(a b c)) (set-cdr! p '(x y)) (cdr p)");
}

test "list? - predicate" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(list? '())");
    try vm.expectEval("#t", "(list? '(a b c))");
    try vm.expectEval("#f", "(list? '(a . b))");
    try vm.expectEval("#f", "(list? 42)");
}

test "make-list - construction" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("3", "(length (make-list 3))");
    try vm.expectEval("0", "(length (make-list 0))");
    try vm.expectEval("(x x x)", "(make-list 3 'x)");
}

test "list - construction" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a b c)", "(list 'a 'b 'c)");
    try vm.expectEval("()", "(list)");
    try vm.expectEval("(1 2 3)", "(list 1 2 3)");
}

test "length - list length" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("0", "(length '())");
    try vm.expectEval("3", "(length '(a b c))");
    try vm.expectEval("1", "(length '(a))");
}

test "car and cdr composition" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("2", "(car (cdr '(1 2 3)))");
    try vm.expectEval("(3)", "(cdr (cdr '(1 2 3)))");
}

test "cons with car and cdr" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(1 2 3)", "(cons (car '(1 2 3)) (cdr '(1 2 3)))");
}

test "append - concatenate lists" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a b c d)", "(append '(a b) '(c d))");
    try vm.expectEval("(a b c d e f)", "(append '(a b) '(c d) '(e f))");
    try vm.expectEval("()", "(append)");
    try vm.expectEval("(a b c)", "(append '(a b c))");
    try vm.expectEval("(a b)", "(append '() '(a b))");
    try vm.expectEval("(a b)", "(append '(a b) '())");
}

test "append - improper append" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(a b . c)", "(append '(a b) 'c)");
    try vm.expectEval("(a b c . d)", "(append '(a b) '(c) 'd)");
}

test "reverse - basic reversal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(c b a)", "(reverse '(a b c))");
    try vm.expectEval("()", "(reverse '())");
    try vm.expectEval("(1)", "(reverse '(1))");
    try vm.expectEval("(3 2 1)", "(reverse '(1 2 3))");
}

test "list-tail - basic operations" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("(c d)", "(list-tail '(a b c d) 2)");
    try vm.expectEval("(a b c d)", "(list-tail '(a b c d) 0)");
    try vm.expectEval("(d)", "(list-tail '(a b c d) 3)");
    try vm.expectEval("()", "(list-tail '(a b c d) 4)");
}

test "list-ref - element access" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("c", "(list-ref '(a b c d) 2)");
    try vm.expectEval("a", "(list-ref '(a b c d) 0)");
    try vm.expectEval("d", "(list-ref '(a b c d) 3)");
}

test "reverse with list-ref" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("c", "(list-ref (reverse '(a b c)) 0)");
    try vm.expectEval("a", "(list-ref (reverse '(a b c)) 2)");
}

test "list-set! - mutation" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("x", "(define lst (list 'a 'b 'c 'd)) (list-set! lst 2 'x) (list-ref lst 2)");
    try vm.expectEval("(a b x d)", "(define lst2 (list 'a 'b 'c 'd)) (list-set! lst2 2 'x) lst2");
    try vm.expectEval("(z b c)", "(define lst3 (list 'a 'b 'c)) (list-set! lst3 0 'z) lst3");
}
