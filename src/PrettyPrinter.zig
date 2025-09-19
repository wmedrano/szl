//! Pretty printer for Scheme values in the interpreter.
//!
//! This module provides a formatter that can display Scheme values in a
//! human-readable format. It integrates with Zig's standard formatting
//! system and handles all value types including lists, symbols, numbers,
//! and procedures.

const std = @import("std");
const testing = std.testing;

const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
const Pair = @import("Pair.zig");
const Procedure = @import("Procedure.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const PrettyPrinter = @This();

vm: *const Vm,
val: Val,

pub const Slice = struct {
    vm: *const Vm,
    vals: []const Val,

    /// Formats a slice of values as a list for pretty printing.
    /// This method enables Slice to be used with std.fmt functions.
    ///
    /// Args:
    ///   self: The Slice instance to format.
    ///   writer: The writer to output the formatted representation to.
    ///
    /// Returns:
    ///   Nothing on success, or a write error if the writer fails.
    pub fn format(
        self: Slice,
        writer: *std.Io.Writer,
    ) error{WriteFailed}!void {
        try writer.writeAll("(");
        for (self.vals, 0..) |val, i| {
            if (i > 0) try writer.writeAll(" ");
            const printer = PrettyPrinter{ .vm = self.vm, .val = val };
            try printer.formatValue(writer, val);
        }
        try writer.writeAll(")");
    }
};

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
    writer: *std.Io.Writer,
) error{WriteFailed}!void {
    try self.formatValue(writer, self.val);
}

/// Internal recursive helper to format different value types.
/// Handles nil, boolean, integer, floating point, symbol, pair, and procedure values.
///
/// Args:
///   self: The PrettyPrinter instance.
///   writer: The writer to output to.
///   val: The value to format.
fn formatValue(self: PrettyPrinter, writer: *std.Io.Writer, val: Val) error{WriteFailed}!void {
    switch (val.repr) {
        .nil => try writer.writeAll("()"),
        .boolean => |b| {
            const s = if (b) "#t" else "#f";
            try writer.print("{s}", .{s});
        },
        .i64 => |n| try writer.print("{d}", .{n}),
        .f64 => |n| try writer.print("{d}", .{n}),
        .symbol => |sym| {
            const symbol = self.vm.interner.get(sym) catch |err| switch (err) {
                error.InvalidId => {
                    try writer.print("#<invalid-symbol:{d}>", .{sym.id});
                    return;
                },
            };
            try writer.writeAll(symbol.data);
        },
        .pair => |handle| {
            const cons = self.vm.inspector().resolve(Pair, handle) catch {
                try writer.writeAll("#<invalid-cons>");
                return;
            };
            try writer.writeAll("(");
            try self.formatValue(writer, cons.car);
            try self.formatCdr(writer, cons.cdr);
            try writer.writeAll(")");
        },
        .procedure => |proc_handle| try self.formatProcedure(writer, proc_handle),
    }
}

/// Formats a procedure value for pretty printing.
///
/// Args:
///   self: The PrettyPrinter instance.
///   writer: The writer to output to.
///   proc_handle: The procedure handle to format.
fn formatProcedure(self: PrettyPrinter, writer: *std.Io.Writer, proc_handle: Handle(Procedure)) error{WriteFailed}!void {
    const proc = self.vm.inspector().resolve(Procedure, proc_handle) catch {
        try writer.print("#<invalid-procedure:{d}>", .{proc_handle.idx});
        return;
    };
    const name_symbol = proc.name orelse {
        try writer.print("#<anonymous-procedure:{d}>", .{proc_handle.idx});
        return;
    };
    const name = self.vm.interner.get(name_symbol) catch |err| switch (err) {
        error.InvalidId => {
            try writer.print("#<invalid-procedure:{d}>", .{name_symbol.id});
            return;
        },
    };
    try writer.print("#<procedure:{s}>", .{name.data});
}

/// Formats the cdr (tail) part of a pair, handling proper list notation.
///
/// Args:
///   self: The PrettyPrinter instance.
///   writer: The writer to output to.
///   cdr: The cdr value to format.
fn formatCdr(self: PrettyPrinter, writer: *std.Io.Writer, cdr: Val) !void {
    switch (cdr.repr) {
        .nil => {}, // Proper list termination, no output needed
        .pair => |handle| {
            const cons = self.vm.inspector().resolve(Pair, handle) catch {
                try writer.writeAll(" . #<invalid-cons>");
                return;
            };
            try writer.writeAll(" ");
            try self.formatValue(writer, cons.car);
            try self.formatCdr(writer, cons.cdr);
        },
        else => {
            try writer.writeAll(" . ");
            try self.formatValue(writer, cdr);
        },
    }
}

test "int formats as int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .i64 = 42 } };

    try testing.expectFmt(
        "42",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "nil formats as empty list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .nil = {} } };

    try testing.expectFmt(
        "()",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "symbol formats as text" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const symbol = try vm.interner.internStatic(Symbol.init("test-symbol"));
    const val = Val{ .repr = .{ .symbol = symbol } };
    try testing.expectFmt(
        "test-symbol",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "cons pair formats with dot" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const car = Val{ .repr = .{ .i64 = 1 } };
    const cdr = Val{ .repr = .{ .i64 = 2 } };
    const cons = Pair{ .car = car, .cdr = cdr };
    const handle = try vm.pairs.put(testing.allocator, cons);
    const val = Val{ .repr = .{ .pair = handle } };

    try testing.expectFmt(
        "(1 . 2)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "proper list formats without dots" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const two = Val{ .repr = .{ .i64 = 2 } };
    const one = Val{ .repr = .{ .i64 = 1 } };

    const cdr_cons = Pair{ .car = two, .cdr = nil };
    const cdr_handle = try vm.pairs.put(testing.allocator, cdr_cons);
    const cdr_val = Val{ .repr = .{ .pair = cdr_handle } };

    const car_cons = Pair{ .car = one, .cdr = cdr_val };
    const car_handle = try vm.pairs.put(testing.allocator, car_cons);
    const val = Val{ .repr = .{ .pair = car_handle } };

    try testing.expectFmt(
        "(1 2)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "nested cons structures format correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const three = Val{ .repr = .{ .i64 = 3 } };
    const two = Val{ .repr = .{ .i64 = 2 } };
    const one = Val{ .repr = .{ .i64 = 1 } };

    const inner_cons = Pair{ .car = two, .cdr = three };
    const inner_handle = try vm.pairs.put(testing.allocator, inner_cons);
    const inner_val = Val{ .repr = .{ .pair = inner_handle } };

    const outer_cons = Pair{ .car = one, .cdr = inner_val };
    const outer_handle = try vm.pairs.put(testing.allocator, outer_cons);
    const val = Val{ .repr = .{ .pair = outer_handle } };

    try testing.expectFmt(
        "(1 2 . 3)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "single element list formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const one = Val{ .repr = .{ .i64 = 42 } };

    const cons = Pair{ .car = one, .cdr = nil };
    const handle = try vm.pairs.put(testing.allocator, cons);
    const val = Val{ .repr = .{ .pair = handle } };

    try testing.expectFmt(
        "(42)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "mixed types in list format correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const symbol = try vm.interner.internStatic(Symbol.init("foo"));
    const symbol_val = Val{ .repr = .{ .symbol = symbol } };
    const number_val = Val{ .repr = .{ .i64 = 42 } };

    const cdr_cons = Pair{ .car = symbol_val, .cdr = nil };
    const cdr_handle = try vm.pairs.put(testing.allocator, cdr_cons);
    const cdr_val = Val{ .repr = .{ .pair = cdr_handle } };

    const car_cons = Pair{ .car = number_val, .cdr = cdr_val };
    const car_handle = try vm.pairs.put(testing.allocator, car_cons);
    const val = Val{ .repr = .{ .pair = car_handle } };

    try testing.expectFmt(
        "(42 foo)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean true formats as #t" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .boolean = true } };

    try testing.expectFmt(
        "#t",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean false formats as #f" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = Val{ .repr = Val.Repr{ .boolean = false } };

    try testing.expectFmt(
        "#f",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean in cons pair formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const car = Val{ .repr = .{ .boolean = true } };
    const cdr = Val{ .repr = .{ .boolean = false } };
    const cons = Pair{ .car = car, .cdr = cdr };
    const handle = try vm.pairs.put(testing.allocator, cons);
    const val = Val{ .repr = .{ .pair = handle } };

    try testing.expectFmt(
        "(#t . #f)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "boolean in mixed type list formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const nil = Val{ .repr = .{ .nil = {} } };
    const bool_val = Val{ .repr = .{ .boolean = true } };
    const number_val = Val{ .repr = .{ .i64 = 42 } };

    const cdr_cons = Pair{ .car = bool_val, .cdr = nil };
    const cdr_handle = try vm.pairs.put(testing.allocator, cdr_cons);
    const cdr_val = Val{ .repr = .{ .pair = cdr_handle } };

    const car_cons = Pair{ .car = number_val, .cdr = cdr_val };
    const car_handle = try vm.pairs.put(testing.allocator, car_cons);
    const val = Val{ .repr = .{ .pair = car_handle } };

    try testing.expectFmt(
        "(42 #t)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "slice formats as list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const vals = [_]Val{
        Val.init(1),
        Val.init(2),
        Val.init(3),
    };

    const slice = Slice{ .vm = &vm, .vals = &vals };

    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{slice},
    );
}
