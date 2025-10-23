const std = @import("std");

const Val = @import("Val.zig");

const Parameter = @This();

/// The current value of the parameter.
value: Val,

pub fn init(initial_value: Val) Parameter {
    return Parameter{
        .value = initial_value,
    };
}

pub fn deinit(self: *Parameter, allocator: std.mem.Allocator) void {
    _ = self;
    _ = allocator;
    // Parameters don't own any allocated memory themselves
}

/// Get the current value of the parameter.
pub fn getValue(self: Parameter) Val {
    return self.value;
}

/// Set the parameter value, applying the converter function if present.
/// The converter must be applied by the caller (VM) since we can't execute functions here.
pub fn setValue(self: *Parameter, new_value: Val) void {
    self.value = new_value;
}
