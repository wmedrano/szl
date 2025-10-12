const std = @import("std");
const testing = std.testing;

const NativeProc = @import("../types/NativeProc.zig");
const Pair = @import("../types/Pair.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

/// car: Returns the first element of a pair
/// (car '(a . b)) => a
/// (car '(a b c)) => a
pub const car = NativeProc.withRawArgs(struct {
    pub const name = "car";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();
        const pair = inspector.asPair(args[0]) catch return .{ .err = error.WrongType };
        return .{ .val = pair.car };
    }
});

/// cdr: Returns the second element of a pair (rest of the list)
/// (cdr '(a . b)) => b
/// (cdr '(a b c)) => (b c)
pub const cdr = NativeProc.withRawArgs(struct {
    pub const name = "cdr";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();
        const pair = inspector.asPair(args[0]) catch return .{ .err = error.WrongType };
        return .{ .val = pair.cdr };
    }
});

/// cons: Constructs a new pair from two values
/// (cons 'a 'b) => (a . b)
/// (cons 'a '(b c)) => (a b c)
pub const cons = NativeProc.withRawArgs(struct {
    pub const name = "cons";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 2) return .{ .err = error.NotImplemented };
        const builder = vm.builder();
        const pair = builder.makePair(args[0], args[1]) catch return .{ .err = error.OutOfMemory };
        return .{ .val = pair };
    }
});

/// pair?: Returns #t if object is a pair, #f otherwise
/// (pair? '(a . b)) => #t
/// (pair? '()) => #f
pub const pair_p = NativeProc.withRawArgs(struct {
    pub const name = "pair?";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const is_pair = switch (args[0].data) {
            .pair => true,
            else => false,
        };
        return .{ .val = Val.initBool(is_pair) };
    }
});

/// null?: Returns #t if object is the empty list, #f otherwise
/// (null? '()) => #t
/// (null? '(a b c)) => #f
pub const null_p = NativeProc.withRawArgs(struct {
    pub const name = "null?";
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const is_null = switch (args[0].data) {
            .empty_list => true,
            else => false,
        };
        return .{ .val = Val.initBool(is_null) };
    }
});

/// set-car!: Mutates the car field of a pair
/// (set-car! pair obj) => unspecified
pub const set_car_b = NativeProc.withRawArgs(struct {
    pub const name = "set-car!";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 2) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();
        const pair = inspector.asPair(args[0]) catch return .{ .err = error.WrongType };
        pair.car = args[1];
        return .{ .val = Val.initEmptyList() };
    }
});

/// set-cdr!: Mutates the cdr field of a pair
/// (set-cdr! pair obj) => unspecified
pub const set_cdr_b = NativeProc.withRawArgs(struct {
    pub const name = "set-cdr!";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 2) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();
        const pair = inspector.asPair(args[0]) catch return .{ .err = error.WrongType };
        pair.cdr = args[1];
        return .{ .val = Val.initEmptyList() };
    }
});

/// list?: Returns #t if obj is a proper list, #f otherwise
/// (list? '(a b c)) => #t
/// (list? '()) => #t
/// (list? '(a . b)) => #f
pub const list_p = NativeProc.withRawArgs(struct {
    pub const name = "list?";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();

        var current = args[0];
        while (true) {
            switch (current.data) {
                .empty_list => return .{ .val = Val.initBool(true) },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return .{ .err = error.UndefinedBehavior };
                    current = pair.cdr;
                },
                else => return .{ .val = Val.initBool(false) },
            }
        }
    }
});

/// make-list: Creates a list of k elements
/// (make-list k) => list of k unspecified values
/// (make-list k fill) => list of k copies of fill
pub const make_list = NativeProc.withRawArgs(struct {
    pub const name = "make-list";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len < 1 or args.len > 2) return .{ .err = error.NotImplemented };

        const k = args[0].asInt() orelse return .{ .err = error.WrongType };
        if (k < 0) return .{ .err = error.NotImplemented };

        const fill = if (args.len == 2) args[1] else Val.initEmptyList();
        const builder = vm.builder();

        var result = Val.initEmptyList();
        var i: i64 = 0;
        while (i < k) : (i += 1) {
            result = builder.makePair(fill, result) catch return .{ .err = error.OutOfMemory };
        }

        return .{ .val = result };
    }
});

/// list: Returns a list of its arguments
/// (list 'a 'b 'c) => (a b c)
/// (list) => ()
pub const list = NativeProc.withRawArgs(struct {
    pub const name = "list";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        const builder = vm.builder();
        const result = builder.makeList(args) catch return .{ .err = error.OutOfMemory };
        return .{ .val = result };
    }
});

/// length: Returns the length of a list
/// (length '(a b c)) => 3
/// (length '()) => 0
pub const length = NativeProc.withRawArgs(struct {
    pub const name = "length";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };
        const inspector = vm.inspector();

        var len: i64 = 0;
        var current = args[0];
        while (true) {
            switch (current.data) {
                .empty_list => return .{ .val = Val.initInt(len) },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return .{ .err = error.UndefinedBehavior };
                    len += 1;
                    current = pair.cdr;
                },
                else => return .{ .err = error.WrongType },
            }
        }
    }
});

/// append: Concatenates lists together
/// (append '(a b) '(c d)) => (a b c d)
/// (append) => ()
/// The last argument need not be a list (improper append)
pub const append = NativeProc.withRawArgs(struct {
    pub const name = "append";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len == 0) return .{ .val = Val.initEmptyList() };
        if (args.len == 1) return .{ .val = args[0] };

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
                        const pair = inspector.handleToPair(h) catch return .{ .err = error.UndefinedBehavior };
                        items.append(vm.allocator(), pair.car) catch return .{ .err = error.OutOfMemory };
                        current = pair.cdr;
                    },
                    else => return .{ .err = error.WrongType },
                }
            }

            // Build this list in reverse onto the result
            const items_slice = items.items;
            var i = items_slice.len;
            while (i > 0) {
                i -= 1;
                result = builder.makePair(items_slice[i], result) catch return .{ .err = error.OutOfMemory };
            }
        }

        return .{ .val = result };
    }
});

/// reverse: Returns a newly allocated list with elements in reverse order
/// (reverse '(a b c)) => (c b a)
/// (reverse '()) => ()
pub const reverse = NativeProc.withRawArgs(struct {
    pub const name = "reverse";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 1) return .{ .err = error.NotImplemented };

        const inspector = vm.inspector();
        const builder = vm.builder();

        var result = Val.initEmptyList();
        var current = args[0];
        while (true) {
            switch (current.data) {
                .empty_list => return .{ .val = result },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return .{ .err = error.UndefinedBehavior };
                    result = builder.makePair(pair.car, result) catch return .{ .err = error.OutOfMemory };
                    current = pair.cdr;
                },
                else => return .{ .err = error.WrongType },
            }
        }
    }
});

/// list-tail: Returns the sublist obtained by omitting the first k elements
/// (list-tail '(a b c d) 2) => (c d)
/// (list-tail list 0) returns list
pub const list_tail = NativeProc.withRawArgs(struct {
    pub const name = "list-tail";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 2) return .{ .err = error.NotImplemented };

        const k = args[1].asInt() orelse return .{ .err = error.WrongType };
        if (k < 0) return .{ .err = error.NotImplemented };

        const inspector = vm.inspector();
        var current = args[0];
        var i: i64 = 0;
        while (i < k) : (i += 1) {
            switch (current.data) {
                .empty_list => return .{ .err = error.NotImplemented },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return .{ .err = error.UndefinedBehavior };
                    current = pair.cdr;
                },
                else => return .{ .err = error.WrongType },
            }
        }

        return .{ .val = current };
    }
});

/// list-ref: Returns the kth element of list (0-indexed)
/// (list-ref '(a b c d) 2) => c
pub const list_ref = NativeProc.withRawArgs(struct {
    pub const name = "list-ref";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 2) return .{ .err = error.NotImplemented };

        const k = args[1].asInt() orelse return .{ .err = error.WrongType };
        if (k < 0) return .{ .err = error.NotImplemented };

        const inspector = vm.inspector();
        var current = args[0];
        var i: i64 = 0;
        while (true) {
            switch (current.data) {
                .empty_list => return .{ .err = error.NotImplemented },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return .{ .err = error.UndefinedBehavior };
                    if (i == k) return .{ .val = pair.car };
                    i += 1;
                    current = pair.cdr;
                },
                else => return .{ .err = error.WrongType },
            }
        }
    }
});

/// list-set!: Mutates the kth element of list to obj
/// (list-set! '(a b c d) 2 'x) mutates list to (a b x d)
/// Returns unspecified value
pub const list_set_b = NativeProc.withRawArgs(struct {
    pub const name = "list-set!";
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        if (args.len != 3) return .{ .err = error.NotImplemented };

        const k = args[1].asInt() orelse return .{ .err = error.WrongType };
        if (k < 0) return .{ .err = error.NotImplemented };
        const new_val = args[2];

        const inspector = vm.inspector();
        var current = args[0];
        var i: i64 = 0;
        while (true) {
            switch (current.data) {
                .empty_list => return .{ .err = error.NotImplemented },
                .pair => |h| {
                    const pair = inspector.handleToPair(h) catch return .{ .err = error.UndefinedBehavior };
                    if (i == k) {
                        pair.car = new_val;
                        return .{ .val = Val.initEmptyList() };
                    }
                    i += 1;
                    current = pair.cdr;
                },
                else => return .{ .err = error.WrongType },
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
