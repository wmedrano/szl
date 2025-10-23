const std = @import("std");

const Context = @import("../Context.zig");
const Val = @import("Val.zig");

const Continuation = @This();

context: Context,

pub fn init(allocator: std.mem.Allocator, ctx: Context) !Continuation {
    var stack = try std.ArrayList(Val).initCapacity(allocator, ctx.stack.items.len);
    errdefer stack.deinit(allocator);
    try stack.appendSlice(allocator, ctx.stack.items);

    var stack_frames = try std.ArrayList(Context.StackFrame).initCapacity(allocator, ctx.stack_frames.items.len);
    errdefer stack_frames.deinit(allocator);
    try stack_frames.appendSlice(allocator, ctx.stack_frames.items);

    var parameter_bindings = try std.ArrayList(Context.ParameterBinding).initCapacity(allocator, ctx.parameter_bindings.items.len);
    errdefer parameter_bindings.deinit(allocator);
    try parameter_bindings.appendSlice(allocator, ctx.parameter_bindings.items);

    return Continuation{
        .context = .{
            .stack = stack,
            .stack_frames = stack_frames,
            .parameter_bindings = parameter_bindings,
        },
    };
}

pub fn deinit(self: *Continuation, allocator: std.mem.Allocator) void {
    self.context.deinit(allocator);
}
