const std = @import("std");

const Diagnostics = @import("../Diagnostics.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Parameter = @import("../types/Parameter.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const make_parameter = NativeProc.withRawArgs(struct {
    pub const name = "make-parameter";
    pub const docstring =
        \\(make-parameter init)
        \\
        \\Creates a new parameter object with initial value `init`.
        \\
        \\Note: Converter functions are not yet supported.
    ;

    pub fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []Val) Vm.Error!Val {
        switch (args.len) {
            1 => {
                const b = vm.builder();
                const initial_value = args[0];
                return try b.makeParameter(initial_value);
            },
            2 => {
                @branchHint(.cold);
                // Special error message for converter argument
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .unsupported_feature = .{
                        .feature_name = "make-parameter with converter argument",
                        .hint = "Use (make-parameter init) with a single argument for now",
                    } });
                }
                return Vm.Error.UncaughtException;
            },
            else => {
                @branchHint(.cold);
                // Wrong number of arguments
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_count = .{
                        .expected = 1,
                        .got = @intCast(args.len),
                        .proc = Val.initNativeProc(&make_parameter),
                    } });
                }
                return Vm.Error.UncaughtException;
            },
        }
    }
});

pub const set_parameter = NativeProc.withRawArgs(struct {
    pub const name = "%sizzle-set-parameter";
    pub const docstring = "";

    pub fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []Val) Vm.Error!Val {
        if (args.len != 2) {
            @branchHint(.cold);
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_count = .{
                    .expected = 2,
                    .got = @intCast(args.len),
                    .proc = Val.initNativeProc(&set_parameter),
                } });
            }
            return Vm.Error.UncaughtException;
        }

        const param = args[0].asParameter() orelse {
            @branchHint(.cold);
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "parameter",
                    .proc = Val.initNativeProc(&set_parameter),
                    .got = args[0],
                    .arg_name = "parameter",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };

        const new_value = args[1];

        // Get mutable access to the parameter
        const param_ptr = vm.objects.parameters.get(param) orelse return Vm.Error.UndefinedBehavior;
        const saved_value = param_ptr.getValue();

        // Save the current value to bindings
        try vm.context.parameter_bindings.append(
            vm.allocator(),
            .{ .parameter = param, .saved_value = saved_value },
        );

        // Set the new value
        const mut_param = @constCast(param_ptr);
        mut_param.setValue(new_value);

        return new_value;
    }
});

test "make-parameter" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test creating a parameter - displays as procedure
    try vm.expectEval("#<procedure:parameter-0>", "(make-parameter 10)");
}

test "make-parameter with converter not supported" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test that converter argument is rejected with helpful error
    const result = vm.evalStr("(make-parameter 10 (lambda (x) x))", null, null);
    try testing.expectError(Vm.Error.UncaughtException, result);
}

test "parameterize macro basic usage" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test basic parameterize
    try vm.expectEval(
        "20",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 20)) (p))
    );
}

test "parameterize macro with multiple parameters" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test multiple parameters
    try vm.expectEval(
        "30",
        \\(define p1 (make-parameter 10))
        \\(define p2 (make-parameter 20))
        \\(parameterize ((p1 5) (p2 25)) (+ (p1) (p2)))
    );
}

test "parameterize macro with multiple body expressions" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test multiple body expressions - should return last value
    try vm.expectEval(
        "40",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 40))
        \\  (p)
        \\  (p)
        \\  (p))
    );
}

test "parameterize macro with empty parameter list" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test empty parameter list
    try vm.expectEval(
        "42",
        \\(parameterize () 42)
    );
}

test "parameterize macro restores values on frame pop" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test that parameter values are restored after parameterize exits
    try vm.expectEval(
        "10",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 20)) (p))
        \\(p)
    );
}

test "parameterize macro nested calls" {
    const testing = std.testing;
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test nested parameterize
    try vm.expectEval(
        "30",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 20))
        \\  (parameterize ((p 30))
        \\    (p)))
    );

    // Verify outer parameterize is still in effect
    try vm.expectEval(
        "20",
        \\(define q (make-parameter 10))
        \\(parameterize ((q 20))
        \\  (parameterize ((q 30))
        \\    (q))
        \\  (q))
    );

    // Verify original value is restored
    try vm.expectEval(
        "10",
        \\(define r (make-parameter 10))
        \\(parameterize ((r 20))
        \\  (parameterize ((r 30))
        \\    (r)))
        \\(r)
    );
}
