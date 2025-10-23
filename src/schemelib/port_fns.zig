const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Port = @import("../types/Port.zig");
const Val = @import("../types/Val.zig");
const PrettyPrinter = @import("../utils/PrettyPrinter.zig");
const Vm = @import("../Vm.zig");

/// Helper function to get the output port from arguments
/// Returns the port value based on argument count:
/// - If explicit_port is provided, returns it
/// - Otherwise, returns the current-output-port parameter
fn getOutputPort(
    vm: *Vm,
    diagnostics: ?*Diagnostics,
) Vm.Error!Val {
    // Get current-output-port parameter from REPL environment
    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(diagnostics);
    const repl_module = try inspector.handleToModule(repl_env);
    const b = vm.builder();
    const current_output_port_symbol = try b.makeStaticSymbolHandle("current-output-port");

    const param_val = repl_module.getBySymbol(current_output_port_symbol) orelse {
        @branchHint(.cold);
        if (diagnostics) |d| {
            d.addDiagnostic(.{ .other = "current-output-port not found" });
        }
        return Vm.Error.UncaughtException;
    };

    // Extract the parameter and get its value
    const param = param_val.asParameter() orelse {
        @branchHint(.cold);
        if (diagnostics) |d| {
            d.addDiagnostic(.{ .other = "current-output-port is not a parameter" });
        }
        return Vm.Error.UncaughtException;
    };
    const val = try inspector.resolveParameter(param);
    return val;
}

/// Parameters for writing to a port
const WriteToPortParams = struct {
    vm: *Vm,
    diagnostics: ?*Diagnostics,
    obj: Val,
    port_val: Val,
    proc: *const NativeProc,
    arg_position: ?u32,
    pretty_options: PrettyPrinter.Options = .{},
};

/// Helper function to write an object to a port
fn writeToPort(params: WriteToPortParams) Vm.Error!void {
    const inspector = params.vm.inspector();
    const port = inspector.asPort(params.port_val) catch {
        @branchHint(.cold);
        if (params.diagnostics) |d| {
            const hint = if (params.arg_position != null)
                "port argument is optional; omit it to use current-output-port"
            else
                "current-output-port parameter must be an output port";
            d.addDiagnostic(.{ .wrong_arg_type = .{
                .expected = "output port",
                .got = params.port_val,
                .proc = Val.initNativeProc(params.proc),
                .arg_name = "port",
                .arg_position = params.arg_position,
                .hint = hint,
            } });
        }
        return Vm.Error.UncaughtException;
    };

    // Write the object using the specified representation
    switch (port.inner) {
        .stdout, .stderr => {
            const file = if (port.inner == .stdout) std.fs.File.stdout() else std.fs.File.stderr();
            var temp = std.io.Writer.Allocating.init(params.vm.allocator());
            defer temp.deinit();

            temp.writer.print("{f}", .{params.vm.pretty(params.obj, params.pretty_options)}) catch {
                @branchHint(.cold);
                if (params.diagnostics) |d| {
                    d.addDiagnostic(.{ .other = "Failed to write to output port" });
                }
                return Vm.Error.UncaughtException;
            };
            file.writeAll(temp.writer.buffered()) catch {
                @branchHint(.cold);
                if (params.diagnostics) |d| {
                    d.addDiagnostic(.{ .other = "Failed to write to output port" });
                }
                return Vm.Error.UncaughtException;
            };
        },
        .stdin => {
            @branchHint(.cold);
            if (params.diagnostics) |d| {
                const hint = if (params.arg_position != null)
                    "port argument is optional; omit it to use current-output-port"
                else
                    "current-output-port parameter must be an output port";
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "output port",
                    .got = params.port_val,
                    .proc = Val.initNativeProc(params.proc),
                    .arg_name = "port",
                    .arg_position = params.arg_position,
                    .hint = hint,
                } });
            }
            return Vm.Error.UncaughtException;
        },
        .null => {},
    }
}

pub const display = NativeProc.withRawArgs(struct {
    pub const name = "display";
    pub const docstring =
        \\(display obj)
        \\(display obj port)
        \\
        \\Writes obj to port using the display representation (human-readable).
        \\The display representation omits quotes from strings and shows simplified
        \\forms of procedures and other objects.
        \\If port is not specified, uses the current output port.
    ;

    pub fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        const obj: Val, const explicit_port: ?Val = switch (args.len) {
            1 => .{ args[0], null },
            2 => .{ args[0], args[1] },
            else => {
                @branchHint(.cold);
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_count = .{
                        .expected = 1,
                        .got = @intCast(args.len),
                        .proc = Val.initNativeProc(&display),
                    } });
                }
                return Vm.Error.UncaughtException;
            },
        };
        const port_val = explicit_port orelse try getOutputPort(vm, diagnostics);
        try writeToPort(.{
            .vm = vm,
            .diagnostics = diagnostics,
            .obj = obj,
            .port_val = port_val,
            .proc = &display,
            .arg_position = if (explicit_port != null) 1 else null,
            .pretty_options = .{ .repr = .display },
        });
        return Val.initUnspecified();
    }
});

pub const newline = NativeProc.withRawArgs(struct {
    pub const name = "newline";
    pub const docstring =
        \\(newline)
        \\(newline port)
        \\
        \\Writes a newline character to port.
        \\If port is not specified, uses the current output port.
    ;

    pub fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        const explicit_port: ?Val = switch (args.len) {
            0 => null,
            1 => args[0],
            else => {
                @branchHint(.cold);
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_count = .{
                        .expected = 0,
                        .got = @intCast(args.len),
                        .proc = Val.initNativeProc(&newline),
                    } });
                }
                return Vm.Error.UncaughtException;
            },
        };

        const port_val = explicit_port orelse try getOutputPort(vm, diagnostics);
        try writeToPort(.{
            .vm = vm,
            .diagnostics = diagnostics,
            .obj = Val.initChar('\n'),
            .port_val = port_val,
            .proc = &newline,
            .arg_position = if (explicit_port != null) 0 else null,
            .pretty_options = .{ .repr = .display },
        });
        return Val.initUnspecified();
    }
});

pub const displayln = NativeProc.withRawArgs(struct {
    pub const name = "displayln";
    pub const docstring =
        \\(displayln obj)
        \\(displayln obj port)
        \\
        \\Writes obj to port using the display representation followed by a newline.
        \\If port is not specified, uses the current output port.
        \\This is a convenience function equivalent to (display obj port) followed by (newline port).
    ;

    pub fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        const obj: Val, const explicit_port: ?Val = switch (args.len) {
            1 => .{ args[0], null },
            2 => .{ args[0], args[1] },
            else => {
                @branchHint(.cold);
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_count = .{
                        .expected = 1,
                        .got = @intCast(args.len),
                        .proc = Val.initNativeProc(&displayln),
                    } });
                }
                return Vm.Error.UncaughtException;
            },
        };
        const port_val = explicit_port orelse try getOutputPort(vm, diagnostics);
        try writeToPort(.{
            .vm = vm,
            .diagnostics = diagnostics,
            .obj = obj,
            .port_val = port_val,
            .proc = &displayln,
            .arg_position = if (explicit_port != null) 1 else null,
            .pretty_options = .{ .repr = .display },
        });
        try writeToPort(.{
            .vm = vm,
            .diagnostics = diagnostics,
            .obj = Val.initChar('\n'),
            .port_val = port_val,
            .proc = &displayln,
            .arg_position = if (explicit_port != null) 1 else null,
            .pretty_options = .{ .repr = .display },
        });
        return Val.initUnspecified();
    }
});

pub const write = NativeProc.withRawArgs(struct {
    pub const name = "write";
    pub const docstring =
        \\(write obj)
        \\(write obj port)
        \\
        \\Writes obj to port using the external representation (machine-readable).
        \\The external representation includes quotes for strings and shows full
        \\type information for procedures and other objects.
        \\If port is not specified, uses the current output port.
    ;

    pub fn impl(vm: *Vm, diagnostics: ?*Diagnostics, args: []const Val) Vm.Error!Val {
        const obj: Val, const explicit_port: ?Val = switch (args.len) {
            1 => .{ args[0], null },
            2 => .{ args[0], args[1] },
            else => {
                @branchHint(.cold);
                if (diagnostics) |d| {
                    d.addDiagnostic(.{ .wrong_arg_count = .{
                        .expected = 1,
                        .got = @intCast(args.len),
                        .proc = Val.initNativeProc(&write),
                    } });
                }
                return Vm.Error.UncaughtException;
            },
        };
        const port_val = explicit_port orelse try getOutputPort(vm, diagnostics);
        try writeToPort(.{
            .vm = vm,
            .diagnostics = diagnostics,
            .obj = obj,
            .port_val = port_val,
            .proc = &write,
            .arg_position = if (explicit_port != null) 1 else null,
            .pretty_options = .{ .repr = .external },
        });
        return Val.initUnspecified();
    }
});

pub const port_p = NativeProc.with1Arg(struct {
    pub const name = "port?";
    pub const docstring =
        \\(port? obj)
        \\
        \\Returns #t if obj is a port, otherwise #f.
    ;

    pub fn impl(vm: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const is_port = inspector.asPort(arg) catch null;
        return Val.initBool(is_port != null);
    }
});

pub const input_port_p = NativeProc.with1Arg(struct {
    pub const name = "input-port?";
    pub const docstring =
        \\(input-port? obj)
        \\
        \\Returns #t if obj is an input port, otherwise #f.
    ;

    pub fn impl(vm: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const port = inspector.asPort(arg) catch {
            return Val.initBool(false);
        };
        const is_input = switch (port.inner) {
            .stdin => true,
            .stdout, .stderr, .null => false,
        };
        return Val.initBool(is_input);
    }
});

pub const output_port_p = NativeProc.with1Arg(struct {
    pub const name = "output-port?";
    pub const docstring =
        \\(output-port? obj)
        \\
        \\Returns #t if obj is an output port, otherwise #f.
    ;

    pub fn impl(vm: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const port = inspector.asPort(arg) catch {
            return Val.initBool(false);
        };
        const is_output = switch (port.inner) {
            .stdout, .stderr, .null => true,
            .stdin => false,
        };
        return Val.initBool(is_output);
    }
});

pub const textual_port_p = NativeProc.with1Arg(struct {
    pub const name = "textual-port?";
    pub const docstring =
        \\(textual-port? obj)
        \\
        \\Returns #t if obj is a textual port, otherwise #f.
        \\Currently all ports in szl are textual.
    ;

    pub fn impl(vm: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const port = inspector.asPort(arg) catch return Val.initBool(false);
        return Val.initBool(port.isTextual());
    }
});

pub const binary_port_p = NativeProc.with1Arg(struct {
    pub const name = "binary-port?";
    pub const docstring =
        \\(binary-port? obj)
        \\
        \\Returns #t if obj is a binary port, otherwise #f.
        \\Currently szl does not support binary ports, so this always returns #f.
    ;

    pub fn impl(vm: *Vm, _: ?*Diagnostics, arg: Val) Vm.Error!Val {
        const inspector = vm.inspector();
        const port = inspector.asPort(arg) catch return Val.initBool(false);
        return Val.initBool(port.isBinary());
    }
});

test "display writes objects to null port" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Display to null port should succeed without output
    try vm.expectEval("#<unspecified>", "(display 42 %szl-null-port)");
    try vm.expectEval("#<unspecified>", "(display \"hello\" %szl-null-port)");
    try vm.expectEval("#<unspecified>", "(display '(1 2 3) %szl-null-port)");
}

test "display with wrong arg count returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(display)");
    try vm.expectError(Vm.Error.UncaughtException, "(display 42 port extra)");
}

test "display with non-port returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(display 42 123)");
    try vm.expectError(Vm.Error.UncaughtException, "(display 42 \"not-a-port\")");
}

test "display with single argument uses current-output-port" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Display with 1 arg should use current-output-port (null port)
    // This should succeed without producing output
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (display 42))",
    );
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (display \"hello\"))",
    );
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (display '(1 2 3)))",
    );
}

test "newline writes to null port" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Newline to null port should succeed without output
    try vm.expectEval("#<unspecified>", "(newline %szl-null-port)");
}

test "newline with no arguments uses current-output-port" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (newline))",
    );
}

test "newline with wrong arg count returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(newline port extra)");
}

test "newline with non-port returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(newline 123)");
    try vm.expectError(Vm.Error.UncaughtException, "(newline \"not-a-port\")");
}

test "displayln writes integer to null port with newline" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Displayln to null port should succeed without output
    try vm.expectEval("#<unspecified>", "(displayln 42 %szl-null-port)");
}

test "displayln writes string to null port with newline" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Displayln to null port should succeed without output
    try vm.expectEval("#<unspecified>", "(displayln \"hello\" %szl-null-port)");
}

test "displayln writes list to null port with newline" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Displayln to null port should succeed without output
    try vm.expectEval("#<unspecified>", "(displayln '(1 2 3) %szl-null-port)");
}

test "displayln with single argument uses current-output-port" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Displayln with 1 arg should use current-output-port (null port)
    // This should succeed without producing output
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (displayln 42))",
    );
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (displayln \"hello\"))",
    );
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (displayln '(1 2 3)))",
    );
}

test "displayln with wrong arg count returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(displayln)");
    try vm.expectError(Vm.Error.UncaughtException, "(displayln 42 port extra)");
}

test "write writes integer to null port" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Write to null port should succeed without output
    try vm.expectEval("#<unspecified>", "(write 42 %szl-null-port)");
}

test "write writes string to null port with quotes" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Write to null port should succeed without output
    // The difference from display is that write includes quotes around strings
    try vm.expectEval("#<unspecified>", "(write \"hello\" %szl-null-port)");
}

test "write with single argument uses current-output-port" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    // Write with 1 arg should use current-output-port (null port)
    // This should succeed without producing output
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (write 42))",
    );
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (write \"hello\"))",
    );
    try vm.expectEval(
        "#<unspecified>",
        "(parameterize ((current-output-port %szl-null-port)) (write '(1 2 3)))",
    );
}

test "write with wrong arg count returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(write)");
    try vm.expectError(Vm.Error.UncaughtException, "(write 42 port extra)");
}

test "write with non-port returns error" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectError(Vm.Error.UncaughtException, "(write 42 123)");
    try vm.expectError(Vm.Error.UncaughtException, "(write 42 \"not-a-port\")");
}

test "port? returns true for ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const null_port = try b.makePort(.null);
    const stdin_port = try b.makePort(.stdin);
    const stdout_port = try b.makePort(.stdout);

    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(null);
    const repl_module = try inspector.handleToModule(repl_env);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("null-port"), null_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdin-port"), stdin_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdout-port"), stdout_port);

    try vm.expectEval("#t", "(port? null-port)");
    try vm.expectEval("#t", "(port? stdin-port)");
    try vm.expectEval("#t", "(port? stdout-port)");
}

test "port? returns false for non-ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(port? 42)");
    try vm.expectEval("#f", "(port? \"string\")");
    try vm.expectEval("#f", "(port? '())");
    try vm.expectEval("#f", "(port? '(1 2 3))");
}

test "input-port? returns true for input ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const stdin_port = try b.makePort(.stdin);

    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(null);
    const repl_module = try inspector.handleToModule(repl_env);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdin-port"), stdin_port);

    try vm.expectEval("#t", "(input-port? stdin-port)");
}

test "input-port? returns false for non-input ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const stdout_port = try b.makePort(.stdout);
    const null_port = try b.makePort(.null);

    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(null);
    const repl_module = try inspector.handleToModule(repl_env);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdout-port"), stdout_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("null-port"), null_port);

    try vm.expectEval("#f", "(input-port? stdout-port)");
    try vm.expectEval("#f", "(input-port? null-port)");
    try vm.expectEval("#f", "(input-port? 42)");
}

test "output-port? returns true for output ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const stdout_port = try b.makePort(.stdout);
    const null_port = try b.makePort(.null);

    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(null);
    const repl_module = try inspector.handleToModule(repl_env);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdout-port"), stdout_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("null-port"), null_port);

    try vm.expectEval("#t", "(output-port? stdout-port)");
    try vm.expectEval("#t", "(output-port? null-port)");
}

test "output-port? returns false for non-output ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const stdin_port = try b.makePort(.stdin);

    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(null);
    const repl_module = try inspector.handleToModule(repl_env);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdin-port"), stdin_port);

    try vm.expectEval("#f", "(output-port? stdin-port)");
    try vm.expectEval("#f", "(output-port? 42)");
}

test "textual-port? returns true for all ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const null_port = try b.makePort(.null);
    const stdin_port = try b.makePort(.stdin);
    const stdout_port = try b.makePort(.stdout);

    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(null);
    const repl_module = try inspector.handleToModule(repl_env);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("null-port"), null_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdin-port"), stdin_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdout-port"), stdout_port);

    try vm.expectEval("#t", "(textual-port? null-port)");
    try vm.expectEval("#t", "(textual-port? stdin-port)");
    try vm.expectEval("#t", "(textual-port? stdout-port)");
}

test "textual-port? returns false for non-ports" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try vm.expectEval("#f", "(textual-port? 42)");
    try vm.expectEval("#f", "(textual-port? \"string\")");
}

test "binary-port? returns false for all values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const null_port = try b.makePort(.null);
    const stdin_port = try b.makePort(.stdin);
    const stdout_port = try b.makePort(.stdout);

    const inspector = vm.inspector();
    const repl_env = try inspector.getReplEnv(null);
    const repl_module = try inspector.handleToModule(repl_env);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("null-port"), null_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdin-port"), stdin_port);
    try repl_module.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("stdout-port"), stdout_port);

    try vm.expectEval("#f", "(binary-port? null-port)");
    try vm.expectEval("#f", "(binary-port? stdin-port)");
    try vm.expectEval("#f", "(binary-port? stdout-port)");
    try vm.expectEval("#f", "(binary-port? 42)");
}
