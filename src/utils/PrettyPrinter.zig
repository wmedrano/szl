const std = @import("std");
const testing = std.testing;

const Closure = @import("../types/Closure.zig");
const Continuation = @import("../types/Continuation.zig");
const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Pair = @import("../types/Pair.zig");
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

vm: *const Vm,
val: Val,

pub fn format(self: PrettyPrinter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (self.val.data) {
        .empty_list => try writer.writeAll("()"),
        .boolean => |b| if (b) try writer.writeAll("#t") else try writer.writeAll("#f"),
        .int => |n| try writer.print("{}", .{n}),
        .float => |f| try writer.print("{d}", .{f}),
        .char => |c| try self.formatChar(writer, c),
        .pair => |h| try self.formatPair(writer, h),
        .string => |h| {
            const string = self.vm.objects.strings.get(h) orelse {
                return writer.print("#<string-{}>", .{h.id});
            };
            try writer.print("\"{s}\"", .{string.asSlice()});
        },
        .symbol => |h| {
            const s = self.vm.objects.symbols.asString(h) orelse {
                return writer.print("#<symbol-{}>", .{h.id});
            };
            try writer.writeAll(s);
        },
        .module => |h| try self.formatModule(writer, h),
        .proc => |h| try self.formatProc(writer, h),
        .closure => |h| try self.formatClosure(writer, h),
        .native_proc => |p| try writer.print("#<procedure:native:{s}>", .{p.name}),
        .vector => |h| try self.formatVector(writer, h),
        .bytevector => |h| try self.formatBytevector(writer, h),
        .continuation => try writer.writeAll("#<procedure:continuation>"),
        .syntax_rules => |h| try self.formatSyntaxRules(writer, h),
        .record => |h| try self.formatRecord(writer, h),
        .record_descriptor => |h| try self.formatRecordDescriptor(writer, h),
    }
}

fn formatChar(_: PrettyPrinter, writer: *std.Io.Writer, c: u21) std.Io.Writer.Error!void {
    // Display named characters with their names
    if (c == 0x0007) {
        try writer.writeAll("#\\alarm");
    } else if (c == 0x0008) {
        try writer.writeAll("#\\backspace");
    } else if (c == 0x007F) {
        try writer.writeAll("#\\delete");
    } else if (c == 0x001B) {
        try writer.writeAll("#\\escape");
    } else if (c == '\n') {
        try writer.writeAll("#\\newline");
    } else if (c == 0x0000) {
        try writer.writeAll("#\\null");
    } else if (c == '\r') {
        try writer.writeAll("#\\return");
    } else if (c == ' ') {
        try writer.writeAll("#\\space");
    } else if (c == '\t') {
        try writer.writeAll("#\\tab");
    } else if (c >= 32 and c <= 126) {
        // Printable ASCII (excluding space which we already handled)
        try writer.print("#\\{u}", .{c});
    } else {
        // Display as hex for non-printable characters
        try writer.print("#\\x{X}", .{c});
    }
}

fn formatPair(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Pair)) std.Io.Writer.Error!void {
    const pair = self.vm.objects.pairs.get(h) orelse {
        return try writer.writeAll("#<invalid-cons>");
    };
    try writer.writeAll("(");
    try self.vm.pretty(pair.car).format(writer);
    var current = pair.cdr;
    while (true) {
        switch (current.data) {
            .empty_list => break,
            .pair => |next_pair_h| {
                const next_pair = self.vm.objects.pairs.get(next_pair_h) orelse {
                    return writer.writeAll(" #<invalid-cons>)");
                };
                try writer.writeAll(" ");
                try self.vm.pretty(next_pair.car).format(writer);
                current = next_pair.cdr;
            },
            else => {
                try writer.writeAll(" . ");
                try self.vm.pretty(current).format(writer);
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
    const sym = self.vm.objects.symbols.asString(proc.name) orelse {
        return try writer.print("#<procedure:{}>", .{proc_h.id});
    };
    try writer.print("#<procedure:{s}>", .{sym});
}

fn formatClosure(
    self: PrettyPrinter,
    writer: *std.Io.Writer,
    closure_h: Handle(Closure),
) std.Io.Writer.Error!void {
    const proc = self.vm.objects.closures.get(closure_h) orelse {
        return try writer.writeAll("#<procedure:closure:invalid>");
    };
    const sym = self.vm.objects.symbols.asString(proc.name) orelse {
        return try writer.print("#<procedure:closure:{}>", .{closure_h.id});
    };
    try writer.print("#<procedure:closure:{s}>", .{sym});
}

fn formatModule(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Module)) std.Io.Writer.Error!void {
    const module = self.vm.objects.modules.get(h) orelse {
        return try writer.writeAll("#<environment:invalid-module>");
    };
    try writer.writeAll("#<environment:module:(");
    for (module.namespace, 0..) |sym, i| {
        if (i > 0) try writer.writeAll(" ");
        const pp = PrettyPrinter{
            .vm = self.vm,
            .val = Val{ .data = .{ .symbol = sym } },
        };
        try pp.format(writer);
    }
    try writer.writeAll(")>");
}

fn formatVector(self: PrettyPrinter, writer: *std.Io.Writer, h: Handle(Vector)) std.Io.Writer.Error!void {
    const vec = self.vm.objects.vectors.get(h) orelse {
        return try writer.writeAll("#<vector:invalid>");
    };
    try writer.writeAll("#(");
    for (vec.items, 0..vec.items.len) |val, idx| {
        if (idx > 0) try writer.writeAll(" ");
        try writer.print("{f}", .{self.vm.pretty(val)});
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
        .{self.vm.pretty(Val.initSymbol(descriptor.name))},
    );

    // Print up to 3 field-value pairs
    const max_fields = @min(3, descriptor.field_names.len);
    if (max_fields > 0) {
        for (descriptor.field_names[0..max_fields], record.fields[0..max_fields]) |field_name, field_value| {
            try writer.print(
                " {f} => {f}",
                .{ self.vm.pretty(Val.initSymbol(field_name)), self.vm.pretty(field_value) },
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
    try self.vm.pretty(Val.initSymbol(descriptor.name)).format(writer);
    try writer.writeAll(">");
}

test "format empty list is empty parens" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectFmt(
        "()",
        "{f}",
        .{vm.pretty(Val.initEmptyList())},
    );
}

test "format int produces int" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectFmt(
        "42",
        "{f}",
        .{vm.pretty(Val.initInt(42))},
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
        .{vm.pretty(try b.makeList(&items))},
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
        .{vm.pretty(try b.makePairs(&items))},
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
        .{vm.pretty(try b.makeList(&outer_items))},
    );
}

test "format symbol produces symbol string" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    try testing.expectFmt(
        "foo",
        "{f}",
        .{vm.pretty(try b.makeStaticSymbol("foo"))},
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
        .{vm.pretty(Val.initModule(env))},
    );
}

test "format character produces character literal" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    try testing.expectFmt("#\\a", "{f}", .{vm.pretty(Val.initChar('a'))});
    try testing.expectFmt("#\\Z", "{f}", .{vm.pretty(Val.initChar('Z'))});
    try testing.expectFmt("#\\space", "{f}", .{vm.pretty(Val.initChar(' '))});
    try testing.expectFmt("#\\newline", "{f}", .{vm.pretty(Val.initChar('\n'))});
    try testing.expectFmt("#\\tab", "{f}", .{vm.pretty(Val.initChar('\t'))});
    try testing.expectFmt("#\\return", "{f}", .{vm.pretty(Val.initChar('\r'))});
    try testing.expectFmt("#\\x3BB", "{f}", .{vm.pretty(Val.initChar(0x3BB))}); // Greek lambda
}
