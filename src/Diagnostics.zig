const std = @import("std");
const testing = std.testing;

const Reader = @import("compiler/Reader.zig");
const Context = @import("Context.zig");
const Module = @import("types/Module.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const PrettyPrinter = @import("utils/PrettyPrinter.zig");
const Vm = @import("Vm.zig");

const Diagnostics = @This();

pub const SyntaxHighlighting = enum {
    color,
    nocolor,
};

pub const StackFrame = struct {
    proc: Val,
};

pub const UndefinedVariableInfo = struct {
    module: Handle(Module),
    symbol: Symbol,
};

pub const WrongArgCountInfo = struct {
    expected: u32,
    got: u32,
    proc: Val,
};

pub const WrongArgTypeInfo = struct {
    expected: []const u8,
    proc: Val,
    got: Val,
    arg_name: ?[]const u8,
    arg_position: ?u32,
    hint: ?[]const u8 = null,
};

pub const InvalidExpressionInfo = struct {
    message: []const u8,
    expr: ?Val = null,
    hint: ?[]const u8 = null,
};

pub const UnsupportedFeatureInfo = struct {
    feature_name: []const u8,
    hint: ?[]const u8 = null,
};

/// Diagnostic information for reader errors
pub const ReadErrorInfo = struct {
    /// The type of error that occurred
    kind: Kind = .unexpected_token,
    /// Line number where the error occurred (1-indexed)
    line: u32 = 0,
    /// Column number where the error occurred (1-indexed)
    column: u32 = 0,
    /// Optional context information about the error
    message: []const u8 = "",
    /// Optional lexeme that caused the error
    lexeme: ?[]const u8 = null,
    /// Optional hint for fixing the error
    hint: ?[]const u8 = null,

    pub const Kind = enum {
        unexpected_eof,
        unexpected_token,
        invalid_number,
        invalid_string,
        invalid_character,
        invalid_bytevector,
        unexpected_dot,
        unexpected_close_paren,
        empty_dot_list,
        multiple_values,
        no_values,
        unsupported_sharpsign_expr,
    };
};

pub const Diagnostic = union(enum) {
    /// No diagnostic (placeholder)
    none,
    /// Error from the reader/parser (syntax error, invalid tokens, etc.)
    reader: ReadErrorInfo,
    /// Runtime undefined behavior detected
    undefined_behavior: []const u8,
    /// Variable not found in module during lookup
    undefined_variable: UndefinedVariableInfo,
    /// Attempted to call a non-procedure value
    not_callable: Val,
    /// Procedure called with incorrect number of arguments
    wrong_arg_count: WrongArgCountInfo,
    /// Procedure called with incorrect argument type
    wrong_arg_type: WrongArgTypeInfo,
    /// Invalid expression during compilation
    invalid_expression: InvalidExpressionInfo,
    /// Unsupported feature attempted
    unsupported_feature: UnsupportedFeatureInfo,
    /// Some other custom message.
    other: []const u8,
};

alloc: std.mem.Allocator,
diagnostics: std.ArrayList(Diagnostic) = .{},
stack_trace: std.ArrayList(StackFrame) = .{},
oom: bool = false,

pub fn init(alloc: std.mem.Allocator) Diagnostics {
    return .{ .alloc = alloc };
}

pub fn deinit(self: *Diagnostics) void {
    self.diagnostics.deinit(self.alloc);
    self.stack_trace.deinit(self.alloc);
    self.oom = false;
}

/// Clear all diagnostics while retaining allocated memory
pub fn reset(self: *Diagnostics) void {
    self.diagnostics.clearRetainingCapacity();
    self.stack_trace.clearRetainingCapacity();
    self.oom = false;
}

/// Add a diagnostic to the collection
pub fn addDiagnostic(self: *Diagnostics, diag: Diagnostic) void {
    self.diagnostics.append(self.alloc, diag) catch {
        self.oom = true;
    };
}

/// Capture the current stack trace from the VM, replacing any existing stack trace
pub fn setStackTrace(self: *Diagnostics, vm: *const Vm) void {
    // Clear existing stack trace
    self.stack_trace.clearRetainingCapacity();

    // Capture new stack trace
    self.stack_trace.ensureTotalCapacity(self.alloc, vm.context.stack_frames.items.len) catch {
        self.oom = true;
        return;
    };
    for (vm.context.stack_frames.items) |ctx_frame| {
        self.stack_trace.appendAssumeCapacity(StackFrame{ .proc = ctx_frame.proc });
    }
}

/// Create a pretty printer for diagnostics
pub fn pretty(self: Diagnostics, vm: *const Vm, syntax_highlighting: SyntaxHighlighting) DiagnosticsPrettyPrinter {
    return DiagnosticsPrettyPrinter{ .diagnostics = self, .vm = vm, .syntax_highlighting = syntax_highlighting };
}

pub const DiagnosticsPrettyPrinter = struct {
    diagnostics: Diagnostics,
    vm: *const Vm,
    syntax_highlighting: SyntaxHighlighting,

    // ANSI color codes
    const Color = struct {
        const reset = "\x1b[0m";
        const bold = "\x1b[1m";
        const dim = "\x1b[2m";
        const red = "\x1b[31m";
        const green = "\x1b[32m";
        const yellow = "\x1b[33m";
    };

    fn colorize(self: DiagnosticsPrettyPrinter, color: []const u8) []const u8 {
        return if (self.syntax_highlighting == .color) color else "";
    }

    pub fn format(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        if (self.diagnostics.oom) {
            try writer.writeAll("Warning: Diagnostics encountered OOM. Some information may be lost.");
        }

        if (self.diagnostics.diagnostics.items.len == 0 and self.diagnostics.stack_trace.items.len == 0) {
            return;
        }

        // Print diagnostics.
        for (self.diagnostics.diagnostics.items) |diag| {
            try self.formatDiagnostic(writer, diag);
            try writer.writeAll("\n");
        }

        // Print stack trace.
        if (self.diagnostics.stack_trace.items.len > 0) {
            try writer.writeAll("\n");
            try self.formatStackTrace(writer);
        }
        try writer.writeAll("\n");
    }

    fn formatStackTrace(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        // Stack Trace Header - Yellow
        try writer.writeAll(self.colorize(Color.yellow));
        try writer.writeAll("Stack trace:");
        try writer.writeAll(self.colorize(Color.reset));

        // Iterate through stack frames in reverse order (most recent first)
        var display_idx: usize = 0;
        var i = self.diagnostics.stack_trace.items.len;
        while (i > 0) {
            i -= 1;
            const frame = self.diagnostics.stack_trace.items[i];

            // Skip frames with empty list as proc - they don't provide useful information
            if (frame.proc.isNull()) {
                continue;
            }

            try writer.writeAll("\n  ");
            // Stack entries - dim with procedure name in bold
            try writer.writeAll(self.colorize(Color.dim));
            if (display_idx == 0) {
                try writer.writeAll("in ");
            } else {
                try writer.writeAll("called from ");
            }
            try writer.writeAll(self.colorize(Color.reset));
            try writer.writeAll(self.colorize(Color.bold));
            try writer.print(
                "{f}",
                .{self.vm.pretty(frame.proc, .{ .repr = .display })},
            );
            try writer.writeAll(self.colorize(Color.reset));
            display_idx += 1;
        }
    }

    fn formatDiagnostic(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, diag: Diagnostic) std.Io.Writer.Error!void {
        switch (diag) {
            .none => try writer.writeAll("No diagnostic"),
            .reader => |r| try self.formatReaderError(writer, r),
            .undefined_behavior => |msg| try self.formatUndefinedBehavior(writer, msg),
            .undefined_variable => |uv| try self.formatUndefinedVariable(writer, uv),
            .not_callable => |val| try self.formatNotCallable(writer, val),
            .wrong_arg_count => |wac| try self.formatWrongArgCount(writer, wac),
            .wrong_arg_type => |wat| try self.formatWrongArgType(writer, wat),
            .invalid_expression => |ie| try self.formatInvalidExpression(writer, ie),
            .unsupported_feature => |uf| try self.formatUnsupportedFeature(writer, uf),
            .other => |msg| try self.formatOther(writer, msg),
        }
    }

    fn formatReaderError(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, r: ReadErrorInfo) std.Io.Writer.Error!void {
        // Location (line:column) - Dim
        try writer.writeAll(self.colorize(Color.dim));
        try writer.print("{}:{}: ", .{ r.line, r.column });
        try writer.writeAll(self.colorize(Color.reset));

        // "error:" label - Red + Bold
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll(self.colorize(Color.bold));
        try writer.writeAll(self.colorize(Color.reset));

        // Error message
        try writer.print(" {s}", .{r.message});

        // Include lexeme if present
        if (r.lexeme) |lex| {
            try writer.writeAll("\n  ");
            try writer.writeAll(self.colorize(Color.dim));
            try writer.writeAll("-->");
            try writer.writeAll(self.colorize(Color.reset));
            try writer.writeAll(" ");
            try writer.writeAll(self.colorize(Color.bold));
            try writer.print("'{s}'", .{lex});
            try writer.writeAll(self.colorize(Color.reset));

            // Include hint on same line if present
            if (r.hint) |hint| {
                try writer.writeAll(" ");
                try writer.writeAll(self.colorize(Color.green));
                try writer.print("{s}", .{hint});
                try writer.writeAll(self.colorize(Color.reset));
            }
        } else if (r.hint) |hint| {
            // Include hint on new line if no lexeme
            try writer.writeAll("\n  ");
            try writer.writeAll(self.colorize(Color.dim));
            try writer.writeAll("hint:");
            try writer.writeAll(self.colorize(Color.reset));
            try writer.writeAll(" ");
            try writer.writeAll(self.colorize(Color.green));
            try writer.print("{s}", .{hint});
            try writer.writeAll(self.colorize(Color.reset));
        }
    }

    fn formatUndefinedBehavior(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, msg: []const u8) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Undefined behavior: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.print("{s}", .{msg});
    }

    fn formatUndefinedVariable(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, uv: UndefinedVariableInfo) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Undefined variable:");
        try writer.writeAll(self.colorize(Color.reset));
        // Field labels - Dim
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("variable: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.print("{f}", .{self.vm.pretty(Val.initSymbol(uv.symbol), .{ .repr = .display })});
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("module:   ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.print("{f}", .{self.vm.pretty(Val.initModule(uv.module), .{ .repr = .display })});
    }

    fn formatNotCallable(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, val: Val) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Not callable:");
        try writer.writeAll(self.colorize(Color.reset));
        // Field label - Dim
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("value: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.print("{f}", .{self.vm.pretty(val, .{})});
        try writer.writeAll("\n  ");
        try writer.writeAll("(expected a procedure)");
    }

    fn formatWrongArgCount(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, wac: WrongArgCountInfo) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Wrong argument count");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(" in call to ");
        try writer.writeAll(self.colorize(Color.bold));
        try writer.print("{f}", .{self.vm.pretty(wac.proc, .{ .repr = .display })});
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(":");
        // Field labels - Dim
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("expected: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(self.colorize(Color.green));
        try writer.print("{}", .{wac.expected});
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("got:      ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(self.colorize(Color.red));
        try writer.print("{}", .{wac.got});
        try writer.writeAll(self.colorize(Color.reset));
    }

    fn formatWrongArgType(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, wat: WrongArgTypeInfo) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Wrong argument type");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(" for ");
        if (wat.arg_position) |pos| {
            try self.formatArgPosition(writer, pos);
        } else {
            try writer.writeAll("an argument");
        }

        if (wat.arg_name) |name| {
            try writer.writeAll(" (");
            try writer.print("'{s}'", .{name});
            try writer.writeAll(")");
        }

        try writer.writeAll(" in call to ");
        try writer.writeAll(self.colorize(Color.bold));
        try writer.print("{f}", .{self.vm.pretty(wat.proc, .{})});
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(":");
        // Field labels - Dim, values colored appropriately
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("expected type: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(self.colorize(Color.green));
        try writer.print("{s}", .{wat.expected});
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("got type:      ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(self.colorize(Color.red));
        try writer.print("{s}", .{self.vm.pretty(wat.got, .{}).typeName()});
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("value:         ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(self.colorize(Color.bold));
        try writer.print("{f}", .{self.vm.pretty(wat.got, .{})});
        try writer.writeAll(self.colorize(Color.reset));

        if (wat.hint) |hint| {
            try writer.writeAll("\n  ");
            try writer.writeAll(self.colorize(Color.dim));
            try writer.writeAll("hint:          ");
            try writer.writeAll(self.colorize(Color.reset));
            try writer.writeAll(self.colorize(Color.green));
            try writer.print("{s}", .{hint});
            try writer.writeAll(self.colorize(Color.reset));
        }
    }

    fn formatArgPosition(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, pos: u32) std.Io.Writer.Error!void {
        _ = self;
        const display_pos = pos + 1; // Convert to 1-indexed for user display
        const suffix = switch (display_pos % 10) {
            1 => if (display_pos % 100 == 11) "th" else "st",
            2 => if (display_pos % 100 == 12) "th" else "nd",
            3 => if (display_pos % 100 == 13) "th" else "rd",
            else => "th",
        };
        try writer.print("{d}{s}", .{ display_pos, suffix });
        try writer.writeAll(" argument");
    }

    fn formatInvalidExpression(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, ie: InvalidExpressionInfo) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Invalid expression: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.print("{s}", .{ie.message});

        if (ie.expr) |expr| {
            try writer.writeAll("\n  ");
            try writer.writeAll(self.colorize(Color.dim));
            try writer.writeAll("expression: ");
            try writer.writeAll(self.colorize(Color.reset));
            try writer.writeAll(self.colorize(Color.bold));
            try writer.print("{f}", .{self.vm.pretty(expr, .{})});
            try writer.writeAll(self.colorize(Color.reset));
        }

        if (ie.hint) |hint| {
            try writer.writeAll("\n  ");
            try writer.writeAll(self.colorize(Color.dim));
            try writer.writeAll("hint: ");
            try writer.writeAll(self.colorize(Color.reset));
            try writer.writeAll(self.colorize(Color.green));
            try writer.print("{s}", .{hint});
            try writer.writeAll(self.colorize(Color.reset));
        }
    }

    fn formatUnsupportedFeature(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, uf: UnsupportedFeatureInfo) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Unsupported feature:");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll("\n  ");
        try writer.writeAll(self.colorize(Color.dim));
        try writer.writeAll("feature: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.writeAll(self.colorize(Color.bold));
        try writer.print("{s}", .{uf.feature_name});
        try writer.writeAll(self.colorize(Color.reset));

        if (uf.hint) |hint| {
            try writer.writeAll("\n  ");
            try writer.writeAll(self.colorize(Color.dim));
            try writer.writeAll("hint: ");
            try writer.writeAll(self.colorize(Color.reset));
            try writer.writeAll(self.colorize(Color.green));
            try writer.print("{s}", .{hint});
            try writer.writeAll(self.colorize(Color.reset));
        }
    }

    fn formatOther(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, msg: []const u8) std.Io.Writer.Error!void {
        // Error message - Red
        try writer.writeAll(self.colorize(Color.red));
        try writer.writeAll("Error: ");
        try writer.writeAll(self.colorize(Color.reset));
        try writer.print("{s}", .{msg});
    }
};

// Test helpers
fn expectStringContains(actual: []const u8, expected_substring: []const u8) !void {
    if (std.mem.indexOf(u8, actual, expected_substring) == null) {
        std.debug.print("\n====== expected to contain: ======\n{s}\n", .{expected_substring});
        std.debug.print("====== actual string: ============\n{s}\n", .{actual});
        std.debug.print("==================================\n", .{});
        return error.TestExpectedStringToContain;
    }
}

fn expectStringDoesNotContain(actual: []const u8, unexpected_substring: []const u8) !void {
    if (std.mem.indexOf(u8, actual, unexpected_substring) != null) {
        std.debug.print("\n====== expected NOT to contain: ==\n{s}\n", .{unexpected_substring});
        std.debug.print("====== actual string: ============\n{s}\n", .{actual});
        std.debug.print("==================================\n", .{});
        return error.TestExpectedStringNotToContain;
    }
}
