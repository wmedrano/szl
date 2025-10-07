const std = @import("std");
const testing = std.testing;

const Module = @import("../types/Module.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Pair = @import("../types/Pair.zig");
const Proc = @import("../types/Proc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vector = @import("../types/Vector.zig");
const Vm = @import("../Vm.zig");

const PrettyPrinter = @This();

vm: *const Vm,
val: Val,

pub fn format(self: PrettyPrinter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (self.val.data) {
        .empty_list => try writer.writeAll("()"),
        .boolean => |b| if (b) try writer.writeAll("#t") else try writer.writeAll("#f"),
        .int => |n| try writer.print("{}", .{n}),
        .pair => |h| try self.formatPair(writer, h),
        .symbol => |h| {
            const s = self.vm.objects.symbols.asSymbol(h) orelse {
                return writer.print("#<symbol-{}>", .{h.id});
            };
            try writer.writeAll(s.string);
        },
        .module => |h| try self.formatModule(writer, h),
        .proc => |h| try self.formatProc(writer, h, null),
        .closure => |h| try self.formatProc(writer, h.proc, h.captures),
        .proc_builtin => |p| try writer.print("#<procedure:{s}>", .{p.name()}),
        .vector => |h| try self.formatVector(writer, h),
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
    captures_h: ?Handle(Vector),
) std.Io.Writer.Error!void {
    const proc = self.vm.objects.procs.get(proc_h) orelse {
        return try writer.writeAll("#<procedure:invalid>");
    };
    const infix = if (captures_h) |_| "*:" else "";
    const sym = self.vm.objects.symbols.asSymbol(proc.name) orelse {
        return try writer.print("#<procedure:{s}{}>", .{ infix, proc_h.id });
    };
    try writer.print("#<procedure:{s}{s}>", .{ infix, sym.string });
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
    for (vec.data.items, 0..vec.data.items.len) |val, idx| {
        if (idx == 0)
            try writer.print("{f}", .{self.vm.pretty(val)})
        else
            try writer.print(" {f}", .{self.vm.pretty(val)});
    }
    try writer.writeAll("#(");
    try writer.writeAll(")");
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
        .{vm.pretty(try b.makeImproperList(&items))},
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
        .{vm.pretty(try b.makeSymbol(Symbol.init("foo")))},
    );
}

test "symbol with same string is eq" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const sym1 = try b.makeSymbol(Symbol.init("foo"));
    const sym2 = try b.makeSymbol(Symbol.init("foo"));

    try testing.expect(sym1.eq(sym2));
}

test "symbol with different identifiers are different" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const sym1 = try b.makeSymbol(Symbol.init("foo"));
    const sym2 = try b.makeSymbol(Symbol.init("foobar"));

    try testing.expect(!sym1.eq(sym2));
}

test "format environment produces namespace in parens" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const b = vm.builder();
    const env = try b.makeEnvironment(&.{
        (try b.makeSymbol(Symbol.init("scheme"))).data.symbol,
        (try b.makeSymbol(Symbol.init("base"))).data.symbol,
    }, &.{});
    try testing.expectFmt(
        "#<environment:module:(scheme base)>",
        "{f}",
        .{vm.pretty(env)},
    );
}
