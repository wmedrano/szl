const std = @import("std");

const Cons = @import("Cons.zig");
const Val = @import("Val.zig");
const Vm = @import("Vm.zig");

const Builder = @This();

vm: *Vm,

pub fn init(vm: *Vm) Builder {
    return Builder{ .vm = vm };
}

pub inline fn makeInt(_: Builder, value: i64) Val {
    return Val{ .data = .{ .int = value } };
}

pub inline fn makeEmptyList(_: Builder) Val {
    return Val{ .data = .{ .empty_list = {} } };
}

pub inline fn makeCons(self: Builder, car: Val, cdr: Val) Vm.Error!Val {
    var cons = try self.vm.allocator().create(Cons);
    cons.car = car;
    cons.cdr = cdr;
    const result = Val{ .data = .{ .pair = cons } };
    try self.vm.objects.append(self.vm.allocator(), result);
    return result;
}

pub inline fn makeList(self: Builder, items: []const Val) Vm.Error!Val {
    var result = self.makeEmptyList();
    var index: usize = items.len;
    while (index > 0) {
        index -= 1;
        result = try self.makeCons(items[index], result);
    }
    return result;
}
