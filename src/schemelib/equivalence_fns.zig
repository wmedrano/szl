const std = @import("std");
const testing = std.testing;

const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const eq_p = NativeProc.withRawArgs(struct {
    pub const name = "eq?";
    pub const docstring =
        \\(eq? obj1 obj2)
        \\
        \\Returns #t if obj1 and obj2 are the same object (pointer equality).
        \\(eq? 'a 'a)     =>  #t
        \\(eq? 42 42)     =>  #t
        \\(eq? '() '())   =>  #t
    ;
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            2 => NativeProc.Result{ .val = Val.initBool(args[0].eq(args[1])) },
            else => NativeProc.Result{
                .wrong_arg_count = .{ .expected = 2, .got = @intCast(args.len) },
            },
        };
    }
});

pub const eqv_p = NativeProc.withRawArgs(struct {
    pub const name = "eqv?";
    pub const docstring =
        \\(eq? obj1 obj2)
        \\
        \\Returns #t if obj1 and obj2 are the same object (pointer equality).
        \\(eq? 'a 'a)     =>  #t
        \\(eq? 42 42)     =>  #t
        \\(eq? '() '())   =>  #t
    ;
    pub inline fn impl(_: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            2 => NativeProc.Result{ .val = Val.initBool(args[0].eq(args[1])) },
            else => NativeProc.Result{
                .wrong_arg_count = .{ .expected = 2, .got = @intCast(args.len) },
            },
        };
    }
});

pub const equal_p = NativeProc.withRawArgs(struct {
    pub const name = "equal?";
    pub const docstring =
        \\(equal? obj1 obj2)
        \\
        \\Returns #t if obj1 and obj2 are structurally equal (deep equality).
        \\Recursively compares pairs, strings, and vectors.
        \\(equal? '(1 2 3) '(1 2 3))  =>  #t
        \\(equal? "hello" "hello")    =>  #t
    ;
    pub inline fn impl(vm: *Vm, args: []const Val) NativeProc.Result {
        return switch (args.len) {
            2 => {
                const result = vm.inspector().isEqual(args[0], args[1]) catch |err| {
                    return NativeProc.Result{ .err = err };
                };
                return NativeProc.Result{ .val = Val.initBool(result) };
            },
            else => NativeProc.Result{
                .wrong_arg_count = .{ .expected = 2, .got = @intCast(args.len) },
            },
        };
    }
});

test "eq? with identical booleans returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? #t #t)");
    try vm.expectEval("#t", "(eq? #f #f)");
}

test "eq? with different booleans returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? #t #f)");
    try vm.expectEval("#f", "(eq? #f #t)");
}

test "eq? with identical numbers returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? 42 42)");
    try vm.expectEval("#t", "(eq? 0 0)");
    try vm.expectEval("#t", "(eq? -5 -5)");
}

test "eq? with different numbers returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 1 2)");
    try vm.expectEval("#f", "(eq? 0 1)");
}

test "eq? with identical symbols returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? 'foo 'foo)");
    try vm.expectEval("#t", "(eq? 'bar 'bar)");
}

test "eq? with different symbols returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? 'foo 'bar)");
}

test "eq? with different types returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(eq? #t 1)");
    try vm.expectEval("#f", "(eq? 0 #f)");
    try vm.expectEval("#f", "(eq? 'symbol 42)");
}

test "eq? with empty lists returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(eq? '() '())");
}

test "eq? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(eq?)", null, null));
}

test "eq? with one argument returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(eq? #t)", null, null));
}

test "eq? with three arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(eq? #t #f #t)", null, null));
}

test "equal? with identical primitives returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? #t #t)");
    try vm.expectEval("#t", "(equal? #f #f)");
    try vm.expectEval("#t", "(equal? 42 42)");
    try vm.expectEval("#t", "(equal? 'foo 'foo)");
    try vm.expectEval("#t", "(equal? '() '())");
}

test "equal? with different primitives returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? #t #f)");
    try vm.expectEval("#f", "(equal? 1 2)");
    try vm.expectEval("#f", "(equal? 'foo 'bar)");
}

test "equal? with equal strings returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? \"hello\" \"hello\")");
    try vm.expectEval("#t", "(equal? \"\" \"\")");
}

test "equal? with different strings returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? \"hello\" \"world\")");
    try vm.expectEval("#f", "(equal? \"a\" \"ab\")");
}

test "equal? with equal lists returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? '(1 2 3) '(1 2 3))");
    try vm.expectEval("#t", "(equal? '(a b c) '(a b c))");
}

test "equal? with different lists returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? '(1 2 3) '(1 2 4))");
    try vm.expectEval("#f", "(equal? '(1 2) '(1 2 3))");
}

test "equal? with nested equal lists returns true" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#t", "(equal? '((1 2) (3 4)) '((1 2) (3 4)))");
    try vm.expectEval("#t", "(equal? '(a (b c) d) '(a (b c) d))");
}

test "equal? with nested different lists returns false" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(equal? '((1 2) (3 4)) '((1 2) (3 5)))");
}

test "equal? with no arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(equal?)", null, null));
}

test "equal? with one argument returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(equal? #t)", null, null));
}

test "equal? with three arguments returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectError(Vm.Error.UncaughtException, vm.evalStr("(equal? #t #f #t)", null, null));
}
