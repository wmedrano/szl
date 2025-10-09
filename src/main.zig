const std = @import("std");

const szl = @import("szl");

pub fn main() !void {
    // Initialize allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    // Read source
    var source = try readInput(gpa.allocator());
    defer source.deinit(gpa.allocator());

    // Initialize stdout
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Evaluate each expression
    var vm = try szl.Vm.init(.{ .allocator = gpa.allocator() });
    defer vm.deinit();
    var reader = vm.read(source.items);
    var expr_count: usize = 0;
    while (try reader.readNext()) |expr| {
        expr_count += 1;
        const result = try vm.evalExpr(expr, null);
        try stdout.print("\x1b[36m${}\x1b[0m => \x1b[32m{f}\x1b[0m\n", .{ expr_count, vm.pretty(result) });
        try stdout.flush();
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
