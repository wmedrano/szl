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

    // Evaluate each expression
    var vm = try szl.Vm.init(.{ .allocator = allocator });
    defer vm.deinit();

    var diagnostics = szl.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    _ = vm.evalStr(source.items, null, &diagnostics) catch |err| {
        const stderr = std.fs.File.stderr();
        // Detect if stderr is a TTY to enable/disable color output
        const use_color = stderr.isTty();
        const syntax_highlighting: szl.Diagnostics.SyntaxHighlighting = if (use_color) .color else .nocolor;

        var temp = std.io.Writer.Allocating.init(allocator);
        defer temp.deinit();
        if (use_color) {
            try temp.writer.print("{s}Error: {}\n", .{ COLOR_RED, err });
            try temp.writer.print("{f}{s}\n", .{ diagnostics.pretty(&vm, syntax_highlighting), COLOR_RESET });
        } else {
            try temp.writer.print("Error: {}\n", .{err});
            try temp.writer.print("{f}\n", .{diagnostics.pretty(&vm, syntax_highlighting)});
        }
        try stderr.writeAll(temp.writer.buffered());
        return err;
    };
}

fn readInput(allocator: std.mem.Allocator) !std.ArrayList(u8) {
    var buf: [1024 * 1024]u8 = undefined;
    var reader = std.fs.File.stdin().reader(&buf);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    _ = try reader.interface.streamRemaining(&writer.writer);
    return writer.toArrayList();
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
                try replEval(allocator, &vm, input_buffer.items, &expr_count);
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

fn replEval(allocator: std.mem.Allocator, vm: *szl.Vm, source: []const u8, expr_count: *usize) !void {
    var diagnostics = szl.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    const stdout = std.fs.File.stdout();
    // Detect if stdout is a TTY to enable/disable color output
    const use_color = stdout.isTty();
    const syntax_highlighting: szl.Diagnostics.SyntaxHighlighting = if (use_color) .color else .nocolor;

    var temp = std.io.Writer.Allocating.init(allocator);
    defer temp.deinit();

    const result = vm.evalStr(source, null, &diagnostics) catch |err| {
        const fatal = blk: switch (err) {
            error.OutOfMemory,
            error.UndefinedBehavior,
            => {
                if (use_color) {
                    _ = temp.writer.print(COLOR_RED ++ "\n{}\n" ++ COLOR_RESET, .{err}) catch {};
                } else {
                    _ = temp.writer.print("\n{}\n", .{err}) catch {};
                }
                break :blk true;
            },
            error.ReadError,
            error.InvalidExpression,
            error.NotImplemented,
            error.UncaughtException,
            => false,
        };
        try temp.writer.print("{f}\n", .{diagnostics.pretty(vm, syntax_highlighting)});
        try stdout.writeAll(temp.writer.buffered());
        if (fatal) return err else return;
    };
    expr_count.* += 1;

    // Don't print unspecified values - they're returned by side-effecting functions
    if (result.data == .unspecified_value) {
        return;
    }

    if (use_color) {
        try temp.writer.print(
            COLOR_CYAN ++ "${}" ++ COLOR_RESET ++ " => " ++ COLOR_GREEN ++ "{f}" ++ COLOR_RESET ++ "\n",
            .{ expr_count.*, vm.pretty(result, .{}) },
        );
    } else {
        try temp.writer.print("${} => {f}\n", .{ expr_count.*, vm.pretty(result, .{}) });
    }
    try stdout.writeAll(temp.writer.buffered());
    temp.clearRetainingCapacity();
}
