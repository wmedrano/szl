const std = @import("std");

const Diagnostics = @import("../Diagnostics.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub const exit = NativeProc{
    .name = "exit",
    .unsafe_impl = &exitImpl,
    .docstring =
    \\(exit)
    \\(exit obj)
    \\
    \\Runs all outstanding dynamic-wind after procedures, terminates the running program, and communicates an exit value to the operating system. If no argument is supplied, or if obj is #t, the exit procedure should communicate to the operating system that the program exited normally. If obj is #f, the exit procedure should communicate to the operating system that the program exited abnormally. Otherwise, exit should translate obj into an appropriate exit value for the operating system, if possible.
    ,
};

fn exitImpl(vm: *Vm, _: ?*Diagnostics, arg_count: u32) Vm.Error!void {
    const exit_code: u8 = switch (arg_count) {
        0 => 0,
        else => blk: {
            const val = vm.context.top() orelse Val.initBool(true);
            if (val.asBool()) |c| {
                break :blk if (c) 0 else 1;
            }
            if (val.asInt()) |c| {
                if (c >= 0 and c <= 255) {
                    break :blk @intCast(c);
                } else {
                    break :blk 1;
                }
            }
            break :blk 0;
        },
    };
    while (vm.context.popStackFrame(.discard)) {}
    std.process.exit(exit_code);
}
