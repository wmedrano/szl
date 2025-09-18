//! Pretty printer for Scheme values in the interpreter.
//!
//! This module provides a formatter that can display Scheme values in a
//! human-readable format. It integrates with Zig's standard formatting
//! system and handles all value types including lists, symbols, and numbers.

const std = @import("std");

const Cons = @import("Cons.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const PrettyPrinter = @This();

vm: *const Vm,
val: Val,

/// Formats the value for pretty printing using the standard format interface.
/// This method enables PrettyPrinter to be used with std.fmt functions.
///
/// Args:
///   self: The PrettyPrinter instance to format.
///   comptime fmt: Format string (unused, but required by std.fmt interface).
///   options: Format options (unused, but required by std.fmt interface).
///   writer: The writer to output the formatted representation to.
///
/// Returns:
///   Nothing on success, or a write error if the writer fails.
pub fn format(
    self: PrettyPrinter,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    try self.formatValue(writer, self.val);
}

/// Internal recursive helper to format different value types.
///
/// Args:
///   self: The PrettyPrinter instance.
///   writer: The writer to output to.
///   val: The value to format.
fn formatValue(self: PrettyPrinter, writer: anytype, val: Val) anyerror!void {
    switch (val.repr) {
        .nil => try writer.writeAll("()"),
        .boolean => |b| {
            const s = if (b) "#t" else "#f";
            try writer.print("{s}", .{s});
        },
        .i64 => |n| try writer.print("{d}", .{n}),
        .symbol => |sym| {
            const symbol = self.vm.interner.get(sym) catch |err| switch (err) {
                error.InvalidId => {
                    try writer.print("<invalid-symbol:{d}>", .{sym.id});
                    return;
                },
            };
            try writer.writeAll(symbol.data);
        },
        .cons => |handle| {
            const cons = self.vm.cons.get(handle) orelse {
                try writer.writeAll("<invalid-cons>");
                return;
            };
            try writer.writeAll("(");
            try self.formatValue(writer, cons.car);
            try self.formatCdr(writer, cons.cdr);
            try writer.writeAll(")");
        },
    }
}

/// Formats the cdr (tail) part of a cons cell, handling proper list notation.
///
/// Args:
///   self: The PrettyPrinter instance.
///   writer: The writer to output to.
///   cdr: The cdr value to format.
fn formatCdr(self: PrettyPrinter, writer: anytype, cdr: Val) !void {
    switch (cdr.repr) {
        .nil => {}, // Proper list termination, no output needed
        .cons => |handle| {
            const cons = self.vm.cons.get(handle) orelse {
                try writer.writeAll(" . <invalid-cons>");
                return;
            };
            try writer.writeAll(" ");
            try self.formatValue(writer, cons.car);
            try self.formatCdr(writer, cons.cdr);
        },
        else => {
            // Improper list (dotted pair)
            try writer.writeAll(" . ");
            try self.formatValue(writer, cdr);
        },
    }
}

test "int formats as int" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .i64 = 42 } };

    try std.testing.expectFmt(
        "42",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "nil formats as empty list" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .nil = {} } };

    try std.testing.expectFmt(
        "()",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "symbol formats as text" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const symbol = try vm.interner.internStatic(Symbol.init("test-symbol"));
    const val = Val{ .repr = .{ .symbol = symbol } };
    try std.testing.expectFmt(
        "test-symbol",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "cons pair formats with dot" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const car = Val{ .repr = .{ .i64 = 1 } };
    const cdr = Val{ .repr = .{ .i64 = 2 } };
    const cons = Cons{ .car = car, .cdr = cdr };
    const handle = try vm.cons.put(std.testing.allocator, cons);
    const val = Val{ .repr = .{ .cons = handle } };

    try std.testing.expectFmt(
        "(1 . 2)",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "proper list formats without dots" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const two = Val{ .repr = .{ .i64 = 2 } };
    const one = Val{ .repr = .{ .i64 = 1 } };

    const cdr_cons = Cons{ .car = two, .cdr = nil };
    const cdr_handle = try vm.cons.put(std.testing.allocator, cdr_cons);
    const cdr_val = Val{ .repr = .{ .cons = cdr_handle } };

    const car_cons = Cons{ .car = one, .cdr = cdr_val };
    const car_handle = try vm.cons.put(std.testing.allocator, car_cons);
    const val = Val{ .repr = .{ .cons = car_handle } };

    try std.testing.expectFmt(
        "(1 2)",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "nested cons structures format correctly" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const three = Val{ .repr = .{ .i64 = 3 } };
    const two = Val{ .repr = .{ .i64 = 2 } };
    const one = Val{ .repr = .{ .i64 = 1 } };

    const inner_cons = Cons{ .car = two, .cdr = three };
    const inner_handle = try vm.cons.put(std.testing.allocator, inner_cons);
    const inner_val = Val{ .repr = .{ .cons = inner_handle } };

    const outer_cons = Cons{ .car = one, .cdr = inner_val };
    const outer_handle = try vm.cons.put(std.testing.allocator, outer_cons);
    const val = Val{ .repr = .{ .cons = outer_handle } };

    try std.testing.expectFmt(
        "(1 2 . 3)",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "single element list formats correctly" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const one = Val{ .repr = .{ .i64 = 42 } };

    const cons = Cons{ .car = one, .cdr = nil };
    const handle = try vm.cons.put(std.testing.allocator, cons);
    const val = Val{ .repr = .{ .cons = handle } };

    try std.testing.expectFmt(
        "(42)",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "mixed types in list format correctly" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const symbol = try vm.interner.internStatic(Symbol.init("foo"));
    const symbol_val = Val{ .repr = .{ .symbol = symbol } };
    const number_val = Val{ .repr = .{ .i64 = 42 } };

    const cdr_cons = Cons{ .car = symbol_val, .cdr = nil };
    const cdr_handle = try vm.cons.put(std.testing.allocator, cdr_cons);
    const cdr_val = Val{ .repr = .{ .cons = cdr_handle } };

    const car_cons = Cons{ .car = number_val, .cdr = cdr_val };
    const car_handle = try vm.cons.put(std.testing.allocator, car_cons);
    const val = Val{ .repr = .{ .cons = car_handle } };

    try std.testing.expectFmt(
        "(42 foo)",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean true formats as #t" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .boolean = true } };

    try std.testing.expectFmt(
        "#t",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean false formats as #f" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .boolean = false } };

    try std.testing.expectFmt(
        "#f",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean in cons pair formats correctly" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const car = Val{ .repr = .{ .boolean = true } };
    const cdr = Val{ .repr = .{ .boolean = false } };
    const cons = Cons{ .car = car, .cdr = cdr };
    const handle = try vm.cons.put(std.testing.allocator, cons);
    const val = Val{ .repr = .{ .cons = handle } };

    try std.testing.expectFmt(
        "(#t . #f)",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean in mixed type list formats correctly" {
    var vm = Vm.init(.{ .allocator = std.testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const bool_val = Val{ .repr = .{ .boolean = true } };
    const number_val = Val{ .repr = .{ .i64 = 42 } };

    const cdr_cons = Cons{ .car = bool_val, .cdr = nil };
    const cdr_handle = try vm.cons.put(std.testing.allocator, cdr_cons);
    const cdr_val = Val{ .repr = .{ .cons = cdr_handle } };

    const car_cons = Cons{ .car = number_val, .cdr = cdr_val };
    const car_handle = try vm.cons.put(std.testing.allocator, car_cons);
    const val = Val{ .repr = .{ .cons = car_handle } };

    try std.testing.expectFmt(
        "(42 #t)",
        "{}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}
