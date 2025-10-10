const std = @import("std");

const szl = @import("szl");
const Tokenizer = @import("szl").Tokenizer;

pub fn main() !void {
    // Initialize allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer _ = gpa.deinit();

    // Check if stdin is a TTY (interactive terminal)
    const stdin = std.fs.File.stdin();
    if (stdin.isTty()) {
        try runRepl(gpa.allocator());
    } else {
        try runScript(gpa.allocator());
    }
}

fn runScript(allocator: std.mem.Allocator) !void {
    // Read source
    var source = try readInput(allocator);
    defer source.deinit(allocator);

    // Initialize stdout
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Evaluate each expression
    var vm = try szl.Vm.init(.{ .allocator = allocator });
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

fn runRepl(allocator: std.mem.Allocator) !void {
    // Initialize stdin and stdout
    var stdin_buffer: [4096]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&stdin_buffer);

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Initialize VM once for the entire session
    var vm = try szl.Vm.init(.{ .allocator = allocator });
    defer vm.deinit();

    // Buffer to accumulate incomplete expressions
    var input_buffer = std.ArrayList(u8).empty;
    defer input_buffer.deinit(allocator);

    var expr_count: usize = 0;

    while (true) {
        // Print prompt
        const prompt = if (input_buffer.items.len == 0) "szl> " else "...> ";
        try stdout.writeAll(prompt);
        try stdout.flush();

        // Read a line
        const line = stdin.interface.takeDelimiterExclusive('\n') catch |err| switch (err) {
            error.EndOfStream => {
                // EOF (Ctrl+D) - exit gracefully
                try stdout.writeAll("\n");
                try stdout.flush();
                return;
            },
            error.StreamTooLong => {
                try stdout.writeAll("\x1b[31mError: Input line too long\x1b[0m\n");
                try stdout.flush();
                continue;
            },
            else => return err,
        };

        // Append line to buffer
        try input_buffer.appendSlice(allocator, line);
        try input_buffer.append(allocator, '\n');

        // Check if expression is complete
        switch (Tokenizer.isComplete(input_buffer.items)) {
            .complete => {},
            .missing_close_paren => continue, // Wait for more input
            .malformed => {
                try stdout.writeAll("\x1b[31mError: Malformed expression (extra closing parenthesis)\x1b[0m\n");
                try stdout.flush();
                input_buffer.clearRetainingCapacity();
                continue;
            },
        }

        // Evaluate the expression
        var reader = vm.read(input_buffer.items);
        defer input_buffer.clearRetainingCapacity();
        while (try reader.readNext()) |expr| {
            expr_count += 1;
            const result = vm.evalExpr(expr, null) catch |err| {
                try stdout.print("\x1b[31mError: {}\x1b[0m\n", .{err});
                try stdout.flush();
                continue;
            };
            try stdout.print("\x1b[36m${}\x1b[0m => \x1b[32m{f}\x1b[0m\n", .{ expr_count, vm.pretty(result) });
            try stdout.flush();
        }
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
