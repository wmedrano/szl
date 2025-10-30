const std = @import("std");
const testing = std.testing;

const ErrorDetails = @import("../types/ErrorDetails.zig");
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

    pub fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []Val) Vm.Error!Val {
        switch (args.len) {
            1 => {
                const b = vm.builder();
                const initial_value = args[0];
                return try b.makeParameter(initial_value);
            },
            2 => {
                @branchHint(.cold);
                // Special error message for converter argument
                diagnostics.addDiagnostic(vm.allocator(),.{ .unsupported_feature = .{
                        .feature_name = "make-parameter with converter argument",
                        .hint = "Use (make-parameter init) with a single argument for now",
                    } });
                return Vm.Error.UncaughtException;
            },
            else => {
                @branchHint(.cold);
                // Wrong number of arguments
                diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_count = .{
                        .expected = 1,
                        .got = @intCast(args.len),
                        .proc = Val.initNativeProc(&make_parameter),
                    } });
                return Vm.Error.UncaughtException;
            },
        }
    }
});

pub const set_parameter = NativeProc.withRawArgs(struct {
    pub const name = "%sizzle-set-parameter";
    pub const docstring = "";

    pub fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []Val) Vm.Error!Val {
        if (args.len != 2) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_count = .{
                .expected = 2,
                .got = @intCast(args.len),
                .proc = Val.initNativeProc(&set_parameter),
            } });
            return Vm.Error.UncaughtException;
        }

        const param = args[0].asParameter() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(),.{ .wrong_arg_type = .{
                .expected = "parameter",
                .proc = Val.initNativeProc(&set_parameter),
                .got = args[0],
                .arg_name = "parameter",
                .arg_position = 0,
            } });
            return Vm.Error.UncaughtException;
        };

        try vm.context.parameter_bindings.append(
            vm.allocator(),
            .{ .parameter = param, .val = args[1] },
        );
        return Val.initUnspecified();
    }
});

test "make-parameter" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test creating a parameter - displays as procedure
    // Note: parameter ID may vary as other parameters are created during VM init
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    const result = try vm.evalStr("(make-parameter 10)", null, &error_details);
    const result_str = try std.fmt.allocPrint(testing.allocator, "{f}", .{vm.pretty(result, .{})});
    defer testing.allocator.free(result_str);
    try testing.expect(std.mem.startsWith(u8, result_str, "#<procedure:parameter-"));
}

test "make-parameter with converter not supported" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test that converter argument is rejected with helpful error
    var error_details = ErrorDetails{};
    defer error_details.deinit(testing.allocator);
    const result = vm.evalStr("(make-parameter 10 (lambda (x) x))", null, &error_details);
    try testing.expectError(Vm.Error.UncaughtException, result);
}

test "parameterize macro basic usage" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test basic parameterize
    try vm.expectEval("20",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 20)) (p))
    );
}

test "parameterize macro with multiple parameters" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test multiple parameters
    try vm.expectEval("30",
        \\(define p1 (make-parameter 10))
        \\(define p2 (make-parameter 20))
        \\(parameterize ((p1 5) (p2 25)) (+ (p1) (p2)))
    );
}

test "parameterize macro with multiple body expressions" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test multiple body expressions - should return last value
    try vm.expectEval("40",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 40))
        \\  (p)
        \\  (p)
        \\  (p))
    );
}

test "parameterize macro with empty parameter list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test empty parameter list
    try vm.expectEval("42",
        \\(parameterize () 42)
    );
}

test "parameterize macro restores values on frame pop" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test that parameter values are restored after parameterize exits
    try vm.expectEval("10",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 20)) (p))
        \\(p)
    );
}

test "parameterize macro nested calls" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Test nested parameterize
    try vm.expectEval("30",
        \\(define p (make-parameter 10))
        \\(parameterize ((p 20))
        \\  (parameterize ((p 30))
        \\    (p)))
    );

    // Verify outer parameterize is still in effect
    try vm.expectEval("20",
        \\(define q (make-parameter 10))
        \\(parameterize ((q 20))
        \\  (parameterize ((q 30))
        \\    (q))
        \\  (q))
    );

    // Verify original value is restored
    try vm.expectEval("10",
        \\(define r (make-parameter 10))
        \\(parameterize ((r 20))
        \\  (parameterize ((r 30))
        \\    (r)))
        \\(r)
    );
}
