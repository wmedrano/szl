//! Pretty printer for Scheme values in the interpreter.
//!
//! This module provides a formatter that can display Scheme values in a
//! human-readable format. It integrates with Zig's standard formatting
//! system and handles all value types including lists, symbols, numbers,
//! and procedures.

const std = @import("std");
const testing = std.testing;

const ByteVector = @import("ByteVector.zig");
const Char = @import("Char.zig");
const object_pool = @import("object_pool.zig");
const Handle = object_pool.Handle;
const Pair = @import("Pair.zig");
const Procedure = @import("Procedure.zig");
const Record = @import("Record.zig");
const String = @import("String.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vector = @import("Vector.zig");
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
        .char => |c| {
            // Format characters with special names appropriately
            switch (c.data) {
                0x07 => try writer.writeAll("#\\alarm"),
                0x08 => try writer.writeAll("#\\backspace"),
                0x7f => try writer.writeAll("#\\delete"),
                0x1b => try writer.writeAll("#\\escape"),
                0x0a => try writer.writeAll("#\\newline"),
                0x00 => try writer.writeAll("#\\null"),
                0x0d => try writer.writeAll("#\\return"),
                0x20 => try writer.writeAll("#\\space"),
                0x09 => try writer.writeAll("#\\tab"),
                else => {
                    // For non-printable characters, use hex format
                    if (c.data < 0x20 or c.data > 0x7E) {
                        try writer.print("#\\x{X:0>2}", .{c.data});
                    } else {
                        try writer.print("#\\{c}", .{c.data});
                    }
                },
            }
        },
        .string => |s| {
            const string = self.vm.inspector().resolve(String, s) catch {
                try writer.writeAll("#<invalid-cons>");
                return;
            };
            try writer.writeByte('"');
            for (string.slice()) |byte| {
                switch (byte) {
                    '"' => try writer.writeAll("\\\""),
                    '\\' => try writer.writeAll("\\\\"),
                    '\n' => try writer.writeAll("\\n"),
                    '\r' => try writer.writeAll("\\r"),
                    '\t' => try writer.writeAll("\\t"),
                    0x08 => try writer.writeAll("\\b"),
                    0x0C => try writer.writeAll("\\f"),
                    0x07 => try writer.writeAll("\\a"),
                    0x0B => try writer.writeAll("\\v"),
                    0x00 => try writer.writeAll("\\0"),
                    else => {
                        // For printable ASCII and valid UTF-8, write directly
                        // For control characters and invalid bytes, use hex escape
                        if (byte >= 0x20 and byte <= 0x7E) {
                            try writer.writeByte(byte);
                        } else if (byte >= 0x80) {
                            // Likely part of UTF-8 sequence, write directly
                            try writer.writeByte(byte);
                        } else {
                            // Control character, use hex escape
                            try writer.print("\\x{X:0>2}", .{byte});
                        }
                    },
                }
            }
            try writer.writeByte('"');
        },
        .vector => |v| {
            const vector = self.vm.inspector().resolve(Vector, v) catch {
                try writer.writeAll("#<invalid-vector>");
                return;
            };
            try writer.writeAll("#(");
            for (vector.slice(), 0..) |value, i| {
                if (i > 0) try writer.writeByte(' ');
                try self.formatValue(writer, value);
            }
            try writer.writeByte(')');
        },
        .bytevector => |v| {
            const bytevector = self.vm.inspector().resolve(ByteVector, v) catch {
                try writer.writeAll("#<invalid-bytevector>");
                return;
            };
            try writer.writeAll("#u8(");
            for (bytevector.slice(), 0..) |byte, i| {
                if (i > 0) try writer.writeByte(' ');
                try writer.print("{}", .{byte});
            }
            try writer.writeByte(')');
        },
        .record => |v| {
            const record = self.vm.inspector().resolve(Record, v) catch {
                try writer.writeAll("#<invalid-record>");
                return;
            };
            const type_descriptor = self.vm.record_type_descriptors.get(record.type_descriptor_handle) orelse {
                try writer.writeAll("#<record-with-invalid-type-descriptor>");
                return;
            };
            const type_name_symbol = self.vm.interner.get(type_descriptor.name) catch {
                try writer.writeAll("#<record-with-invalid-type>");
                return;
            };
            try writer.print("#<{s}", .{type_name_symbol.data});
            for (0..record.fieldCount()) |i| {
                try writer.writeByte(' ');
                const field_value = record.getField(i) catch {
                    try writer.writeAll("#<invalid-field>");
                    continue;
                };
                try self.formatValue(writer, field_value);
            }
            try writer.writeAll(">");
        },
        .record_type_descriptor => |v| {
            const descriptor = self.vm.inspector().resolve(Record.RecordTypeDescriptor, v) catch {
                try writer.writeAll("#<invalid-record-type-descriptor>");
                return;
            };
            const type_name_symbol = self.vm.interner.get(descriptor.name) catch {
                try writer.writeAll("#<record-type-descriptor-with-invalid-name>");
                return;
            };
            try writer.print("#<record-type:{s}", .{type_name_symbol.data});
            if (descriptor.fieldCount() > 0) {
                try writer.writeAll(" fields:");
                for (0..descriptor.fieldCount()) |i| {
                    const field_name = descriptor.getFieldName(i) catch {
                        try writer.writeAll(" #<invalid-field-name>");
                        continue;
                    };
                    const field_name_symbol = self.vm.interner.get(field_name) catch {
                        try writer.writeAll(" #<invalid-field-name>");
                        continue;
                    };
                    try writer.print(" {s}", .{field_name_symbol.data});
                }
            }
            try writer.writeAll(">");
        },
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
        .proc => |proc_handle| try self.formatProcedure(writer, proc_handle),
        .native_proc => |proc| try writer.print("#<procedure:{s}>", .{proc.name}),
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
    const handle = try vm.builder().buildHandle(cons);
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
    const cdr_handle = try vm.builder().buildHandle(cdr_cons);
    const cdr_val = Val{ .repr = .{ .pair = cdr_handle } };

    const car_cons = Pair{ .car = one, .cdr = cdr_val };
    const car_handle = try vm.builder().buildHandle(car_cons);
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
    const inner_handle = try vm.builder().buildHandle(inner_cons);
    const inner_val = Val{ .repr = .{ .pair = inner_handle } };

    const outer_cons = Pair{ .car = one, .cdr = inner_val };
    const outer_handle = try vm.builder().buildHandle(outer_cons);
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
    const handle = try vm.builder().buildHandle(cons);
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
    const cdr_handle = try vm.builder().buildHandle(cdr_cons);
    const cdr_val = Val{ .repr = .{ .pair = cdr_handle } };

    const car_cons = Pair{ .car = number_val, .cdr = cdr_val };
    const car_handle = try vm.builder().buildHandle(car_cons);
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
    const handle = try vm.builder().buildHandle(cons);
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
    const cdr_handle = try vm.builder().buildHandle(cdr_cons);
    const cdr_val = Val{ .repr = .{ .pair = cdr_handle } };

    const car_cons = Pair{ .car = number_val, .cdr = cdr_val };
    const car_handle = try vm.builder().buildHandle(car_cons);
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

test "string formats with quotes and escapes" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(String.initStatic("hello world"));

    try testing.expectFmt(
        "\"hello world\"",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "string with escape characters formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(String.initStatic("hello\nworld\t!"));

    try testing.expectFmt(
        "\"hello\\nworld\\t!\"",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "string with quotes and backslashes formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(String.initStatic("say \"hello\" and use \\"));

    try testing.expectFmt(
        "\"say \\\"hello\\\" and use \\\\\"",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "empty string formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const val = try vm.builder().build(String.init());

    try testing.expectFmt(
        "\"\"",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "string with control characters formats with hex escapes" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const data = [_]u8{ 'h', 'i', 0x01, 0x1F, '!' };
    const val = try vm.builder().build(try String.initFromSlice(testing.allocator, &data));

    try testing.expectFmt(
        "\"hi\\x01\\x1F!\"",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "vector formats with #() syntax" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const values = [_]Val{ Val.init(1), Val.init(2), Val.init(3) };
    const vector = try Vector.initFromSlice(testing.allocator, &values);
    const val = try vm.builder().build(vector);

    try testing.expectFmt(
        "#(1 2 3)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "empty vector formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const vector = Vector.init();
    const vector_handle = try vm.builder().buildHandle(vector);
    const val = Val{ .repr = .{ .vector = vector_handle } };

    try testing.expectFmt(
        "#()",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "vector with mixed types formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const values = [_]Val{ Val.init(42), Val.init(true), Val.init({}), try vm.toVal(String.initStatic("hello")) };
    const vector = try Vector.initFromSlice(testing.allocator, &values);
    const vector_handle = try vm.builder().buildHandle(vector);
    const val = Val{ .repr = .{ .vector = vector_handle } };

    try testing.expectFmt(
        "#(42 #t () \"hello\")",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "bytevector formats with #u8() syntax" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const bytes = [_]u8{ 0x01, 0x02, 0x03 };
    const bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    const val = try vm.builder().build(bytevector);

    try testing.expectFmt(
        "#u8(1 2 3)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "empty bytevector formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const bytevector = ByteVector.init();
    const bytevector_handle = try vm.builder().buildHandle(bytevector);
    const val = Val{ .repr = .{ .bytevector = bytevector_handle } };

    try testing.expectFmt(
        "#u8()",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "bytevector with various values formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const bytes = [_]u8{ 0, 42, 128, 255 };
    const bytevector = try ByteVector.initFromSlice(testing.allocator, &bytes);
    const bytevector_handle = try vm.builder().buildHandle(bytevector);
    const val = Val{ .repr = .{ .bytevector = bytevector_handle } };

    try testing.expectFmt(
        "#u8(0 42 128 255)",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "record formats with type name and field values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const age = try vm.interner.internStatic(Symbol.init("age"));
    const field_names = [_]Symbol.Interned{ first_name, age };
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const values = [_]Val{ Val.init(42), Val.init(true) };
    const record = try Record.initWithValues(testing.allocator, &vm, descriptor_handle, &values);

    const record_handle = try vm.builder().buildHandle(record);
    const val = Val{ .repr = .{ .record = record_handle } };

    try testing.expectFmt(
        "#<person 42 #t>",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "empty record formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("empty"));
    const field_names = [_]Symbol.Interned{};
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const record = try Record.init(testing.allocator, &vm, descriptor_handle);

    const record_handle = try vm.builder().buildHandle(record);
    const val = Val{ .repr = .{ .record = record_handle } };

    try testing.expectFmt(
        "#<empty>",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "record type descriptor formats with type name and field names" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const age = try vm.interner.internStatic(Symbol.init("age"));
    const field_names = [_]Symbol.Interned{ first_name, age };
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const val = Val{ .repr = .{ .record_type_descriptor = descriptor_handle } };

    try testing.expectFmt(
        "#<record-type:person fields: first-name age>",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}

test "empty record type descriptor formats correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("empty"));
    const field_names = [_]Symbol.Interned{};
    const descriptor = try Record.RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const val = Val{ .repr = .{ .record_type_descriptor = descriptor_handle } };

    try testing.expectFmt(
        "#<record-type:empty>",
        "{f}",
        .{PrettyPrinter{ .vm = &vm, .val = val }},
    );
}
