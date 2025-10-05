const std = @import("std");
const testing = std.testing;

const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const PrettyPrinter = @This();

vm: *const Vm,
val: Val,

pub fn format(self: PrettyPrinter, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (self.val.data) {
        .empty_list => try writer.writeAll("()"),
        .int => |n| try writer.print("{}", .{n}),
        .pair => |h| {
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
        },
        .symbol => |h| {
            const s = self.vm.objects.symbols.asSymbol(h) orelse {
                return writer.print("#<symbol-{}>", .{h.id});
            };
            try writer.writeAll(s.string);
        },
        .module => |h| {
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
        },
        .proc => |h| {
            const proc = self.vm.objects.procs.get(h) orelse {
                return try writer.writeAll("#<procedure:invalid>");
            };
            const sym = self.vm.objects.symbols.asSymbol(proc.name) orelse {
                return try writer.print("#<procedure:{}>", .{h.id});
            };
            try writer.print("#<procedure:{s}>", .{sym.string});
        },
        .proc_builtin => |p| try writer.print("#<procedure:{s}>", .{p.name()}),
    }
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
