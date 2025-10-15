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
    const source = try readInput(allocator);
    defer allocator.free(source);

    // Evaluate each expression
    var vm = try szl.Vm.init(.{ .allocator = allocator });
    defer vm.deinit();

    var diagnostics = szl.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    _ = vm.evalStr(source, null, &diagnostics) catch |err| {
        const stderr = std.fs.File.stderr();
        var buf: [4096]u8 = undefined;
        var msg = try std.fmt.bufPrint(&buf, "{s}Error: {}\n", .{ COLOR_RED, err });
        try stderr.writeAll(msg);
        msg = try std.fmt.bufPrint(&buf, "{f}{s}\n", .{ diagnostics.pretty(&vm), COLOR_RESET });
        try stderr.writeAll(msg);
        return err;
    };
}

fn readInput(allocator: std.mem.Allocator) ![]const u8 {
    const stdin = std.fs.File.stdin();
    defer stdin.close();
    var input_buffer: [1024]u8 = undefined;
    var reader = stdin.reader(&input_buffer);
    return reader.interface.readAlloc(allocator, 0);
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
        const line = try editor.readLine(prompt) orelse return;
        try input_buffer.appendSlice(allocator, line);
        try input_buffer.append(allocator, '\n');

        // Check if expression is complete
        switch (Tokenizer.isComplete(input_buffer.items)) {
            .complete => {
                defer input_buffer.clearRetainingCapacity();
                try evaluateAndPrint(allocator, &vm, input_buffer.items, &expr_count);
            },
            .missing_close_paren => {},
            .malformed => {
                const stdout = std.fs.File.stdout();
                try stdout.writeAll(
                    COLOR_RED ++ "Error: Malformed expression (extra closing parenthesis)" ++ COLOR_RESET ++ "\n",
                );
                input_buffer.clearRetainingCapacity();
                continue;
            },
        }
    }
}

fn evaluateAndPrint(allocator: std.mem.Allocator, vm: *szl.Vm, source: []const u8, expr_count: *usize) !void {
    var diagnostics = szl.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    var reader = szl.Reader.init(vm, source);
    var diagnostic = szl.Reader.Diagnostic{};
    const stdout = std.fs.File.stdout();
    var buf: [4096]u8 = undefined;

    while (true) {
        const expr = reader.readNext(&diagnostic) catch |err| switch (err) {
            szl.Reader.Error.OutOfMemory => return err,
            szl.Reader.Error.NotImplemented, szl.Reader.Error.ReadError => {
                const msg = try std.fmt.bufPrint(&buf, COLOR_RED ++ "{f}\n\n\n" ++ COLOR_RESET, .{diagnostic});
                try stdout.writeAll(msg);
                return;
            },
        } orelse return;

        const result = vm.evalExpr(expr, null, &diagnostics) catch |err| {
            var msg = try std.fmt.bufPrint(&buf, COLOR_RED ++ "Error: {}\n\n" ++ COLOR_RESET, .{err});
            try stdout.writeAll(msg);
            msg = try std.fmt.bufPrint(&buf, "{f}\n\n\n", .{diagnostics.pretty(vm)});
            try stdout.writeAll(msg);
            continue;
        };
        expr_count.* += 1;
        const msg = try std.fmt.bufPrint(
            &buf,
            COLOR_CYAN ++ "${}" ++ COLOR_RESET ++ " => " ++ COLOR_GREEN ++ "{f}" ++ COLOR_RESET ++ "\n",
            .{ expr_count.*, vm.pretty(result) },
        );
        try stdout.writeAll(msg);
    }
}
