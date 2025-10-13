const std = @import("std");

const Reader = @import("compiler/Reader.zig");
const Context = @import("Context.zig");
const Module = @import("types/Module.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Symbol = @import("types/Symbol.zig");
const Val = @import("types/Val.zig");
const PrettyPrinter = @import("utils/PrettyPrinter.zig");
const Vm = @import("Vm.zig");

const Diagnostics = @This();

pub const Diagnostic = union(enum) {
    /// No diagnostic (placeholder)
    none,
    /// Error from the reader/parser (syntax error, invalid tokens, etc.)
    reader: Reader.Diagnostic,
    /// Runtime undefined behavior detected
    undefined_behavior: []const u8,
    /// Variable not found in module during lookup
    undefined_variable: struct { module: Handle(Module), symbol: Symbol },
    /// Attempted to call a non-procedure value
    not_callable: Val,
    /// Procedure called with incorrect number of arguments
    wrong_arg_count: struct { expected: u32, got: u32, proc: Val },
    /// Procedure called with incorrect argument type
    wrong_arg_type: struct { expected: []const u8, proc: Val, got: Val, arg_name: ?[]const u8, arg_position: ?u32 },
};

pub const StackFrame = struct {
    depth: usize,
    proc: Val,
};

alloc: std.mem.Allocator,
diagnostics: std.ArrayList(Diagnostic) = .{},
stack_frames: std.ArrayList(StackFrame) = .{},
oom: bool = false,

pub fn init(alloc: std.mem.Allocator) Diagnostics {
    return .{ .alloc = alloc };
}

pub fn deinit(self: *Diagnostics) void {
    self.diagnostics.deinit(self.alloc);
    self.stack_frames.deinit(self.alloc);
    self.oom = false;
}

/// Clear all diagnostics while retaining allocated memory
pub fn reset(self: *Diagnostics) void {
    self.diagnostics.clearRetainingCapacity();
    self.stack_frames.clearRetainingCapacity();
    self.oom = false;
}

/// Append a reader diagnostic to the collection
pub fn appendReader(self: *Diagnostics, diag: Reader.Diagnostic) void {
    self.diagnostics.append(self.alloc, .{ .reader = diag }) catch {
        self.oom = true;
    };
}

/// Append an undefined behavior diagnostic to the collection
pub fn appendUndefinedBehavior(self: *Diagnostics, message: []const u8) void {
    self.diagnostics.append(self.alloc, .{ .undefined_behavior = message }) catch {
        self.oom = true;
    };
}

/// Append an undefined variable diagnostic to the collection
pub fn appendUndefinedVariable(self: *Diagnostics, info: struct {
    module: Handle(Module),
    symbol: Symbol,
}) void {
    self.diagnostics.append(
        self.alloc,
        .{ .undefined_variable = .{ .module = info.module, .symbol = info.symbol } },
    ) catch {
        self.oom = true;
    };
}

/// Append a not callable diagnostic to the collection
pub fn appendNotCallable(self: *Diagnostics, val: Val) void {
    self.diagnostics.append(self.alloc, .{ .not_callable = val }) catch {
        self.oom = true;
    };
}

/// Append a wrong argument count diagnostic to the collection
pub fn appendWrongArgCount(self: *Diagnostics, info: struct {
    expected: u32,
    got: u32,
    proc: Val,
}) void {
    self.diagnostics.append(
        self.alloc,
        .{ .wrong_arg_count = .{ .expected = info.expected, .got = info.got, .proc = info.proc } },
    ) catch {
        self.oom = true;
    };
}

/// Append a wrong argument type diagnostic to the collection
pub fn appendWrongArgType(self: *Diagnostics, info: struct {
    expected: []const u8,
    proc: Val,
    got: Val,
    arg_name: ?[]const u8 = null,
    arg_position: ?u32 = null,
}) void {
    self.diagnostics.append(
        self.alloc,
        .{ .wrong_arg_type = .{ .expected = info.expected, .proc = info.proc, .got = info.got, .arg_name = info.arg_name, .arg_position = info.arg_position } },
    ) catch {
        self.oom = true;
    };
}

/// Set all stack frames from a Context to build a complete backtrace.
/// This includes the current stack frame (idx 0) and all frames in the stack_frames history.
/// Frames are appended in order from most recent (current) to oldest (bottom of stack).
/// Clears any existing stack frames before appending.
pub fn setStackFrames(self: *Diagnostics, ctx: Context) void {
    self.stack_frames.clearRetainingCapacity();

    // Append current stack frame (depth 0)
    self.stack_frames.append(self.alloc, .{ .depth = 0, .proc = ctx.stack_frame.proc }) catch {
        self.oom = true;
    };

    // Append historical stack frames in reverse order (most recent first)
    var idx: usize = 1;
    var frame_idx = ctx.stack_frames.items.len;
    while (frame_idx > 0) {
        frame_idx -= 1;
        self.stack_frames.append(self.alloc, .{ .depth = idx, .proc = ctx.stack_frames.items[frame_idx].proc }) catch {
            self.oom = true;
        };
        idx += 1;
    }
}

/// Create a pretty printer for diagnostics
pub fn pretty(self: Diagnostics, vm: *const Vm) DiagnosticsPrettyPrinter {
    return DiagnosticsPrettyPrinter{ .diagnostics = self, .vm = vm };
}

pub const DiagnosticsPrettyPrinter = struct {
    diagnostics: Diagnostics,
    vm: *const Vm,

    pub fn format(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        if (self.diagnostics.oom) {
            try writer.writeAll("Warning: Diagnostics encountered OOM. Some information may be lost.");
        }

        if (self.diagnostics.diagnostics.items.len == 0 and self.diagnostics.stack_frames.items.len == 0) {
            try writer.writeAll("Warning: No diagnostics provided.");
            return;
        }

        for (self.diagnostics.diagnostics.items) |diag| {
            try writer.writeAll("\n");
            try self.formatDiagnostic(writer, diag);
        }

        // Print stack frames after diagnostics
        if (self.diagnostics.stack_frames.items.len > 0) {
            try writer.writeAll("\n\nStack trace:");
            for (self.diagnostics.stack_frames.items) |sf| {
                // Skip frames with empty list as proc - they don't provide useful information
                if (sf.proc.isNull()) continue;

                try writer.writeAll("\n  ");
                if (sf.depth == 0) {
                    try writer.print("in {f}", .{self.vm.pretty(sf.proc)});
                } else {
                    try writer.print("called from {f}", .{self.vm.pretty(sf.proc)});
                }
            }
        }
    }

    fn formatDiagnostic(self: DiagnosticsPrettyPrinter, writer: *std.Io.Writer, diag: Diagnostic) std.Io.Writer.Error!void {
        switch (diag) {
            .none => try writer.writeAll("No diagnostic"),
            .reader => |r| {
                try writer.print("Reader error at line {}, col {}: {s}", .{
                    r.line,
                    r.column,
                    r.message,
                });
            },
            .undefined_behavior => |msg| {
                try writer.print("Undefined behavior: {s}", .{msg});
            },
            .undefined_variable => |uv| {
                try writer.print(
                    "Undefined variable:\n  variable: {f}\n  module:   {f}",
                    .{ self.vm.pretty(Val.initSymbol(uv.symbol)), self.vm.pretty(Val.initModule(uv.module)) },
                );
            },
            .not_callable => |val| {
                try writer.print("Not callable:\n  value: {f}\n  (expected a procedure)", .{self.vm.pretty(val)});
            },
            .wrong_arg_count => |wac| {
                try writer.print(
                    "Wrong argument count in call to {f}:\n  expected: {}\n  got:      {}",
                    .{ self.vm.pretty(wac.proc), wac.expected, wac.got },
                );
            },
            .wrong_arg_type => |wat| {
                try writer.print("Wrong argument type in call to {f}:\n", .{self.vm.pretty(wat.proc)});

                try writer.writeAll("  ");
                if (wat.arg_position) |pos| {
                    const display_pos = pos + 1; // Convert to 1-indexed for user display
                    try writer.print("argument {d}", .{display_pos});
                } else {
                    try writer.writeAll("an argument");
                }

                if (wat.arg_name) |name| {
                    try writer.print(" ('{s}')", .{name});
                }

                try writer.print("\n  expected: {s}\n  got:      {s} {f}", .{
                    wat.expected,
                    self.vm.pretty(wat.got).typeName(),
                    self.vm.pretty(wat.got),
                });
            },
        }
    }
};
