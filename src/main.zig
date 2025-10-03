const std = @import("std");

const szl = @import("szl");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    var source = try readInput(gpa.allocator());
    defer source.deinit(gpa.allocator());

    var vm = try szl.Vm.init(.{ .allocator = gpa.allocator() });
    defer vm.deinit();
    var reader = vm.read(source.items);
    while (try reader.readNext()) |expr| {
        try output(expr);
    }
}

fn readInput(allocator: std.mem.Allocator) !std.ArrayList(u8) {
    var ret = std.ArrayList(u8){};
    errdefer ret.deinit(allocator);

    const stdin = std.fs.File.stdin();
    defer stdin.close();
    var input_buffer: [1024]u8 = undefined;
    var reader = stdin.reader(&input_buffer);
    while (!reader.atEnd()) {
        const len = reader.read(&input_buffer) catch |err| switch (err) {
            error.EndOfStream => 0,
            else => break,
        };
        try ret.appendSlice(allocator, input_buffer[0..len]);
    }
    return ret;
}

fn output(expr: szl.Val) !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Expression: {f}\n", .{expr});
    try stdout.flush();
}
