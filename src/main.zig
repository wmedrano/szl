const std = @import("std");

const szl = @import("szl");
const Tokenizer = @import("szl").Tokenizer;

const LineEditor = @import("utils/LineEditor.zig");

// ANSI color codes
const COLOR_CYAN = "\x1b[36m";
const COLOR_GREEN = "\x1b[32m";
const COLOR_RED = "\x1b[31m";
const COLOR_RESET = "\x1b[0m";

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
        try stdout.print(
            COLOR_CYAN ++ "${}" ++ COLOR_RESET ++ " => " ++ COLOR_GREEN ++ "{f}" ++ COLOR_RESET ++ "\n",
            .{ expr_count, vm.pretty(result) },
        );
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

fn runRepl(allocator: std.mem.Allocator) !void {
    // Initialize VM once for the entire session
    var vm = try szl.Vm.init(.{ .allocator = allocator });
    defer vm.deinit();

    // Initialize line editor
    var editor = try LineEditor.init(allocator);
    defer editor.deinit();

    // Buffer to accumulate incomplete expressions
    var input_buffer = std.ArrayList(u8).empty;
    defer input_buffer.deinit(allocator);

    var expr_count: usize = 0;

    while (true) {
        // Get prompt based on state
        const prompt = if (input_buffer.items.len == 0) "szl> " else "...> ";

        // Read a line using the line editor
        const line = try editor.readLine(prompt);
        if (line == null) return;
        try input_buffer.appendSlice(allocator, line.?);
        try input_buffer.append(allocator, '\n');

        // Check if expression is complete
        switch (Tokenizer.isComplete(input_buffer.items)) {
            .complete => {},
            .missing_close_paren => continue, // Wait for more input
            .malformed => {
                const stdout = std.fs.File.stdout();
                try stdout.writeAll(
                    COLOR_RED ++ "Error: Malformed expression (extra closing parenthesis)" ++ COLOR_RESET ++ "\n",
                );
                input_buffer.clearRetainingCapacity();
                continue;
            },
        }

        // Evaluate the expression
        var reader = vm.read(input_buffer.items);
        defer input_buffer.clearRetainingCapacity();
        const stdout = std.fs.File.stdout();
        var buf: [512]u8 = undefined;
        while (reader.readNext() catch |err| {
            const msg = try std.fmt.bufPrint(&buf, COLOR_RED ++ "Error: {}\n" ++ COLOR_RESET, .{err});
            try stdout.writeAll(msg);
            continue;
        }) |expr| {
            const result = vm.evalExpr(expr, null) catch |err| {
                const msg = try std.fmt.bufPrint(&buf, COLOR_RED ++ "Error: {}\n" ++ COLOR_RESET, .{err});
                try stdout.writeAll(msg);
                continue;
            };
            expr_count += 1;
            const msg = try std.fmt.bufPrint(
                &buf,
                COLOR_CYAN ++ "${}" ++ COLOR_RESET ++ " => " ++ COLOR_GREEN ++ "{f}" ++ COLOR_RESET ++ "\n",
                .{ expr_count, vm.pretty(result) },
            );
            try stdout.writeAll(msg);
        }
    }
}
