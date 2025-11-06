const std = @import("std");
const testing = std.testing;

const Box = @import("../types/Box.zig");
const Continuation = @import("../types/Continuation.zig");
const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Pair = @import("../types/Pair.zig");
const Port = @import("../types/Port.zig");
const Proc = @import("../types/Proc.zig");
const Record = @import("../types/Record.zig");
const String = @import("../types/String.zig");
const Symbol = @import("../types/Symbol.zig");
const SyntaxRules = @import("../types/SyntaxRules.zig");
const Val = @import("../types/Val.zig");
const Vector = Val.Vector;
const ByteVector = Val.ByteVector;
const Vm = @import("../Vm.zig");

const PrettyPrinter = @This();

pub const Representation = enum {
    /// External representation (write) - shows full type information
    /// Examples: "#<procedure:native:+>", "\"hello\""
    external,
    /// Display representation (display) - simpler, human-readable format
    /// Examples: "+", "hello"
    display,
};

pub const Options = struct {
    repr: Representation = .external,
    max_depth: usize = 8,
};

vm: *const Vm,
val: Val,
options: Options,
depth: usize = 0,

/// Creates a new PrettyPrinter for a child value with incremented depth
fn child(self: PrettyPrinter, val: Val) PrettyPrinter {
    return PrettyPrinter{
        .vm = self.vm,
        .val = val,
        .options = self.options,
        .depth = self.depth + 1,
    };
}

pub fn format(self: PrettyPrinter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    // Check depth limit to prevent infinite loops with cyclic structures
    if (self.depth >= self.options.max_depth) {
        return writer.writeAll("...");
    }

    switch (self.val.data) {
        .empty_list => try writer.writeAll("()"),
        .unspecified_value => try writer.writeAll("#<unspecified>"),
        .boolean => |b| if (b) try writer.writeAll("#t") else try writer.writeAll("#f"),
        .int => |n| try writer.print("{}", .{n}),
        .rational => |r| try writer.print("{}/{}", .{ r.numerator, r.denominator }),
        .float => |f| {
            const as_int: i64 = @intFromFloat(f);
            if (@as(f64, @floatFromInt(as_int)) == f)
                try writer.print("{d:.1}", .{f})
            else
                try writer.print("{d:}", .{f});
        },
        .char => |c| try self.formatChar(writer, c),
        .pair => |h| try self.formatPair(writer, h),
        .string => |h| {
            const string = self.vm.objects.strings.get(h) orelse {
                return writer.print("#<string-{}>", .{h.id});
            };
            switch (self.options.repr) {
                .display => try writer.writeAll(string.asSlice()),
                .external => try writer.print("\"{s}\"", .{string.asSlice()}),
            }
        },
        .symbol => |sym| try writer.print("{f}", .{self.vm.objects.symbols.formatted(sym)}),
        .module => |h| try self.formatModule(writer, h),
        .proc => |h| try self.formatProc(writer, h),
        .native_proc => |p| {
            switch (self.options.repr) {
                .display => try writer.writeAll(p.name),
                .external => try writer.print("#<procedure:native:{s}>", .{p.name}),
            }
        },
        .vector => |h| try self.formatVector(writer, h),
        .bytevector => |h| try self.formatBytevector(writer, h),
        .box => |h| try self.formatBox(writer, h),
        .continuation => switch (self.options.repr) {
            .display => try writer.writeAll("#<continuation>"),
            .external => try writer.writeAll("#<procedure:continuation>"),
        },
        .syntax_rules => |h| try self.formatSyntaxRules(writer, h),
        .record => |h| try self.formatRecord(writer, h),
        .record_descriptor => |h| try self.formatRecordDescriptor(writer, h),
        .parameter => |h| switch (self.options.repr) {
            .display => try writer.writeAll("#<parameter>"),
            .external => try writer.print("#<procedure:parameter-{}>", .{h.id}),
        },
        .port => |h| try self.formatPort(writer, h),
        .error_details => |h| switch (self.options.repr) {
            .display => try writer.writeAll("#<error-details>"),
            .external => try writer.print("#<error-details-{}>", .{h.id}),
        },
    }
}

fn formatChar(self: PrettyPrinter, writer: *std.io.Writer, c: u21) std.io.Writer.Error!void {
    switch (self.options.repr) {
        .display => {
            // display prints just the character (no prefix)
            switch (c) {
                '\n' => try writer.writeAll("\n"),
                else => {
                    var buf: [4]u8 = undefined;
                    const len = std.unicode.utf8Encode(c, &buf) catch return writer.writeAll("<?>");
                    try writer.writeAll(buf[0..len]);
                },
            }
        },
        .external => {
            // external: keep named and escaped forms
            switch (c) {
                0x0007 => try writer.writeAll("#\\alarm"),
                0x0008 => try writer.writeAll("#\\backspace"),
                0x007F => try writer.writeAll("#\\delete"),
                0x001B => try writer.writeAll("#\\escape"),
                '\n' => try writer.writeAll("#\\newline"),
                0x0000 => try writer.writeAll("#\\null"),
                '\r' => try writer.writeAll("#\\return"),
                ' ' => try writer.writeAll("#\\space"),
                '\t' => try writer.writeAll("#\\tab"),
                else => {
                    if (c >= 32 and c <= 126)
                        try writer.print("#\\{u}", .{c})
                    else
                        try writer.print("#\\x{X}", .{c});
                },
            }
        },
    }
}

fn formatPair(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Pair)) std.Io.Writer.Error!void {
    const pair = self.vm.objects.pairs.get(h) orelse {
        return try writer.writeAll("#<invalid-cons>");
    };
    try writer.writeAll("(");
    try self.child(pair.car).format(writer);
    var current = pair.cdr;
    while (true) {
        switch (current.data) {
            .empty_list => break,
            .pair => |next_pair_h| {
                const next_pair = self.vm.objects.pairs.get(next_pair_h) orelse {
                    return writer.writeAll(" #<invalid-cons>)");
                };
                try writer.writeAll(" ");
                try self.child(next_pair.car).format(writer);
                current = next_pair.cdr;
            },
            else => {
                try writer.writeAll(" . ");
                try self.child(current).format(writer);
                break;
            },
        }
    }
    try writer.writeAll(")");
}

fn formatProc(
    self: PrettyPrinter,
    writer: *std.Io.Writer,
    proc_h: Handle(Proc),
) std.Io.Writer.Error!void {
    const proc = self.vm.objects.procs.get(proc_h) orelse {
        return try writer.writeAll("#<procedure:invalid>");
    };
    const sym = self.vm.objects.symbols.formatted(proc.name);
    switch (self.options.repr) {
        .display => try writer.print("{f}", .{sym}),
        .external => try writer.print("#<procedure:{f}>", .{sym}),
    }
}

fn formatModule(
    self: PrettyPrinter,
    writer: *std.io.Writer,
    h: Handle(Module),
) std.io.Writer.Error!void {
    const module = self.vm.objects.modules.get(h) orelse {
        return try writer.writeAll("#<environment:invalid-module>");
    };

    const parts = switch (self.options.repr) {
        .display => .{ "(", ")" },
        .external => .{ "#<environment:module:(", ")>" },
    };

    const prefix = parts[0];
    const suffix = parts[1];

    try writer.writeAll(prefix);
    var first = true;
    for (module.namespace) |sym| {
        if (!first) try writer.writeAll(" ");
        first = false;

        const pp = PrettyPrinter{
            .vm = self.vm,
            .val = Val{ .data = .{ .symbol = sym } },
            .options = self.options,
        };
        try pp.format(writer);
    }

    try writer.writeAll(suffix);
}

fn formatVector(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Vector)) std.Io.Writer.Error!void {
    const vec = self.vm.objects.vectors.get(h) orelse {
        return try writer.writeAll("#<vector:invalid>");
    };
    try writer.writeAll("#(");
    for (vec.items, 0..vec.items.len) |val, idx| {
        if (idx > 0) try writer.writeAll(" ");
        try writer.print("{f}", .{self.vm.pretty(val, .{})});
    }
    try writer.writeAll(")");
}

fn formatBytevector(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(ByteVector)) std.Io.Writer.Error!void {
    const bv = self.vm.objects.bytevectors.get(h) orelse {
        return try writer.writeAll("#<bytevector:invalid>");
    };
    try writer.writeAll("#u8(");
    for (bv.items, 0..bv.items.len) |byte, idx| {
        if (idx > 0) try writer.writeAll(" ");
        try writer.print("{}", .{byte});
    }
    try writer.writeAll(")");
}

fn formatBox(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Box)) std.Io.Writer.Error!void {
    const box = self.vm.objects.boxes.get(h) orelse {
        return try writer.writeAll("#<box:invalid>");
    };
    try writer.print("{f}", .{self.vm.pretty(box.value, self.options)});
}

fn formatSyntaxRules(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(SyntaxRules)) std.Io.Writer.Error!void {
    _ = self.vm.objects.syntax_rules.get(h) orelse {
        return try writer.print("#<syntax-rules:invalid-{}>", .{h.id});
    };
    return writer.writeAll("#<syntax-rules>");
}

fn formatRecord(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Record)) std.Io.Writer.Error!void {
    const record = self.vm.objects.records.get(h) orelse {
        return try writer.print("#<record:invalid-{}>", .{h.id});
    };
    const descriptor = self.vm.objects.record_descriptors.get(record.descriptor) orelse {
        return try writer.print("#<record:invalid-descriptor-{}>", .{h.id});
    };

    // Print record type name
    try writer.print(
        "#<record:{f}",
        .{self.vm.pretty(Val.initSymbol(descriptor.name), .{})},
    );

    // Print up to 3 field-value pairs
    const max_fields = @min(3, descriptor.field_names.len);
    if (max_fields > 0) {
        for (descriptor.field_names[0..max_fields], record.fields[0..max_fields]) |field_name, field_value| {
            try writer.print(
                " {f} => {f}",
                .{ self.vm.pretty(Val.initSymbol(field_name), .{}), self.vm.pretty(field_value, .{}) },
            );
        }
        // Add ellipsis if there are more fields
        if (descriptor.field_names.len > 3) {
            try writer.writeAll(" ...");
        }
    }

    try writer.writeAll(">");
}

fn formatRecordDescriptor(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Record.Descriptor)) std.Io.Writer.Error!void {
    const descriptor = self.vm.objects.record_descriptors.get(h) orelse {
        return try writer.print("#<record-descriptor:invalid-{}>", .{h.id});
    };
    try writer.writeAll("#<record-descriptor:");
    try self.vm.pretty(Val.initSymbol(descriptor.name), .{}).format(writer);
    try writer.writeAll(">");
}

fn formatPort(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Port)) std.Io.Writer.Error!void {
    const port = self.vm.objects.ports.get(h) orelse {
        return try writer.print("#<port:invalid-{}>", .{h.id});
    };

    switch (self.options.repr) {
        .display => {
            switch (port.inner) {
                .null => try writer.writeAll("#<null port>"),
                .stdin => try writer.writeAll("#<stdin>"),
                .stdout => try writer.writeAll("#<stdout>"),
                .stderr => try writer.writeAll("#<stderr>"),
            }
        },
        .external => {
            switch (port.inner) {
                .null => try writer.writeAll("#<port:null>"),
                .stdin => try writer.writeAll("#<port:input:stdin>"),
                .stdout => try writer.writeAll("#<port:output:stdout>"),
                .stderr => try writer.writeAll("#<port:output:stderr>"),
            }
        },
    }
}

/// Returns a human-readable type name for the value.
/// For records, includes the record type name if available.
pub fn typeName(self: PrettyPrinter) []const u8 {
    return switch (self.val.data) {
        .empty_list => "empty list",
        .unspecified_value => "unspecified value",
        .boolean => "boolean",
        .int => "integer",
        .rational => "rational",
        .float => "float",
        .char => "character",
        .symbol => "symbol",
        .string => "string",
        .pair => "pair",
        .vector => "vector",
        .bytevector => "bytevector",
        .box => "box",
        .module => "module",
        .proc, .native_proc => "procedure",
        .continuation => "continuation",
        .syntax_rules => "syntax-rules",
        .record => "record",
        .record_descriptor => "record descriptor",
        .parameter => "parameter",
        .port => "port",
        .error_details => "error details",
    };
}

test "format empty list is empty parens" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectFmt(
        "()",
        "{f}",
        .{vm.pretty(Val.initEmptyList(), .{})},
    );
}

test "format int produces int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectFmt(
        "42",
        "{f}",
        .{vm.pretty(Val.initInt(42), .{})},
    );
}

test "format proper list produces parens surrounded list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ Val.initInt(1), Val.initInt(2), Val.initInt(3) };
    try testing.expectFmt(
        "(1 2 3)",
        "{f}",
        .{vm.pretty(try b.makeList(&items), .{})},
    );
}

test "format improper list places dot before last element" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const items = [_]Val{ Val.initInt(1), Val.initInt(2), Val.initInt(3) };
    try testing.expectFmt(
        "(1 2 . 3)",
        "{f}",
        .{vm.pretty(try b.makePairs(&items), .{})},
    );
}

test "format nested list formats the nested list" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const inner_items = [_]Val{ Val.initInt(2), Val.initInt(3) };
    const inner_list = try b.makeList(&inner_items);
    const outer_items = [_]Val{ Val.initInt(1), inner_list };
    try testing.expectFmt(
        "(1 (2 3))",
        "{f}",
        .{vm.pretty(try b.makeList(&outer_items), .{})},
    );
}

test "format symbol produces symbol string" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    try testing.expectFmt(
        "foo",
        "{f}",
        .{vm.pretty(try b.makeStaticSymbol("foo"), .{})},
    );
}

test "symbol with same string is eq" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const sym1 = try b.makeStaticSymbol("foo");
    const sym2 = try b.makeStaticSymbol("foo");

    try testing.expect(sym1.eq(sym2));
}

test "symbol with different identifiers are different" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const sym1 = try b.makeStaticSymbol("foo");
    const sym2 = try b.makeStaticSymbol("foobar");

    try testing.expect(!sym1.eq(sym2));
}

test "format environment produces namespace in parens" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const env = try b.makeEnvironment(&.{
        (try b.makeStaticSymbolHandle("scheme")),
        (try b.makeStaticSymbolHandle("base")),
    }, &.{});
    try testing.expectFmt(
        "#<environment:module:(scheme base)>",
        "{f}",
        .{vm.pretty(Val.initModule(env), .{})},
    );
}

test "format character produces character literal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectFmt("#\\a", "{f}", .{vm.pretty(Val.initChar('a'), .{})});
    try testing.expectFmt("#\\Z", "{f}", .{vm.pretty(Val.initChar('Z'), .{})});
    try testing.expectFmt("#\\space", "{f}", .{vm.pretty(Val.initChar(' '), .{})});
    try testing.expectFmt("#\\newline", "{f}", .{vm.pretty(Val.initChar('\n'), .{})});
    try testing.expectFmt("#\\tab", "{f}", .{vm.pretty(Val.initChar('\t'), .{})});
    try testing.expectFmt("#\\return", "{f}", .{vm.pretty(Val.initChar('\r'), .{})});
    try testing.expectFmt("#\\x3BB", "{f}", .{vm.pretty(Val.initChar(0x3BB), .{})}); // Greek lambda
}

test "format port shows meaningful port type information" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();

    // Test stdin port
    const port_stdin = try b.makePort(.stdin);
    try testing.expectFmt("#<port:input:stdin>", "{f}", .{vm.pretty(port_stdin, .{})});
    try testing.expectFmt("#<stdin>", "{f}", .{vm.pretty(port_stdin, .{ .repr = .display })});

    // Test stdout port
    const port_stdout = try b.makePort(.stdout);
    try testing.expectFmt("#<port:output:stdout>", "{f}", .{vm.pretty(port_stdout, .{})});
    try testing.expectFmt("#<stdout>", "{f}", .{vm.pretty(port_stdout, .{ .repr = .display })});

    // Test null port
    const port_null = try b.makePort(.null);
    try testing.expectFmt("#<port:null>", "{f}", .{vm.pretty(port_null, .{})});
    try testing.expectFmt("#<null port>", "{f}", .{vm.pretty(port_null, .{ .repr = .display })});
}
