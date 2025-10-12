const std = @import("std");
const testing = std.testing;

const Vm = @import("../Vm.zig");
const Handle = @import("object_pool.zig").Handle;
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const Record = @This();

pub const Descriptor = struct {
    name: Symbol,
    field_names: []const Symbol,

    pub fn deinit(self: *Descriptor, allocator: std.mem.Allocator) void {
        allocator.free(self.field_names);
    }
};

descriptor: Handle(Descriptor),
fields: []Val,

pub fn deinit(self: *Record, allocator: std.mem.Allocator) void {
    allocator.free(self.fields);
}

pub fn getField(self: Record, vm: *Vm, field_index: usize) Vm.Error!Val {
    const descriptor = try vm.inspector().handleToRecordDescriptor(self.descriptor);
    if (field_index >= descriptor.field_names.len) {
        return Vm.Error.UndefinedBehavior;
    }
    return self.fields[field_index];
}

pub fn setField(self: *Record, vm: *Vm, field_index: usize, val: Val) Vm.Error!void {
    const descriptor = try vm.inspector().handleToRecordDescriptor(self.descriptor);
    if (field_index >= descriptor.field_names.len) {
        return Vm.Error.UndefinedBehavior;
    }
    self.fields[field_index] = val;
}
