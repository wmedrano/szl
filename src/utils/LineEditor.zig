const std = @import("std");

const LineEditor = @This();

// Control characters
const CTRL_A = 1;
const CTRL_C = 3;
const CTRL_D = 4;
const CTRL_E = 5;
const BACKSPACE = 127;
const BACKSPACE_ALT = 8;
const ENTER = 13;
const ESC = 27;

// ANSI escape sequences
const ESCAPE_CLEAR_LINE = "\r\x1b[K";
const ESCAPE_CURSOR_LEFT = "\x1b[{d}D";

allocator: std.mem.Allocator,
buffer: std.ArrayList(u8),
cursor_pos: usize,
original_termios: ?std.posix.termios = null,

pub fn init(allocator: std.mem.Allocator) !LineEditor {
    return .{
        .allocator = allocator,
        .buffer = std.ArrayList(u8).empty,
        .cursor_pos = 0,
        .original_termios = null,
    };
}

pub fn deinit(self: *LineEditor) void {
    self.buffer.deinit(self.allocator);
    self.restoreTerminal() catch {};
}

pub fn readLine(self: *LineEditor, prompt: []const u8) !?[]const u8 {
    // Setup raw mode
    try self.enableRawMode();
    defer self.restoreTerminal() catch {};

    // Clear the buffer
    self.buffer.clearRetainingCapacity();
    self.cursor_pos = 0;

    // Get stdin and stdout
    const stdin = std.fs.File.stdin();
    const stdout = std.fs.File.stdout();

    // Print prompt
    try stdout.writeAll(prompt);

    var read_buf: [1]u8 = undefined;

    while (true) {
        const n = try stdin.read(&read_buf);
        if (n == 0) {
            // EOF
            try stdout.writeAll("\n");
            return null;
        }

        const c = read_buf[0];

        // Handle control characters
        if (c == ENTER) {
            try stdout.writeAll("\r\n");
            return self.buffer.items;
        } else if (c == CTRL_C) {
            try stdout.writeAll("^C\n");
            return null;
        } else if (c == CTRL_D) {
            if (self.buffer.items.len == 0) {
                try stdout.writeAll("\n");
                return null;
            }
            // Otherwise ignore
        } else if (c == CTRL_A) {
            try self.moveCursorTo(0, prompt);
        } else if (c == CTRL_E) {
            try self.moveCursorTo(self.buffer.items.len, prompt);
        } else if (c == BACKSPACE or c == BACKSPACE_ALT) {
            if (self.cursor_pos > 0) {
                _ = self.buffer.orderedRemove(self.cursor_pos - 1);
                self.cursor_pos -= 1;
                try self.refreshLine(prompt);
            }
        } else if (c == ESC) {
            // Read next two bytes
            var seq_buf: [2]u8 = undefined;
            const seq_n = try stdin.read(&seq_buf);
            if (seq_n == 2 and seq_buf[0] == '[') {
                switch (seq_buf[1]) {
                    'A' => {}, // Up arrow - ignore for now
                    'B' => {}, // Down arrow - ignore for now
                    'C' => { // Right arrow
                        if (self.cursor_pos < self.buffer.items.len) {
                            try self.moveCursorTo(self.cursor_pos + 1, prompt);
                        }
                    },
                    'D' => { // Left arrow
                        if (self.cursor_pos > 0) {
                            try self.moveCursorTo(self.cursor_pos - 1, prompt);
                        }
                    },
                    else => {},
                }
            }
        } else if (c >= 32 and c < 127) { // Printable character
            try self.buffer.insert(self.allocator, self.cursor_pos, c);
            self.cursor_pos += 1;
            try self.refreshLine(prompt);
        }
    }
}

fn enableRawMode(self: *LineEditor) !void {
    const stdin = std.fs.File.stdin();

    // Save original terminal settings
    self.original_termios = try std.posix.tcgetattr(stdin.handle);

    var raw = self.original_termios.?;

    // Disable canonical mode and echo
    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    raw.lflag.ISIG = false;
    raw.lflag.IEXTEN = false;

    // Disable input processing
    raw.iflag.IXON = false;
    raw.iflag.ICRNL = false;
    raw.iflag.BRKINT = false;
    raw.iflag.INPCK = false;
    raw.iflag.ISTRIP = false;

    // Disable output processing
    raw.oflag.OPOST = false;

    // Set control characters
    raw.cc[@intFromEnum(std.posix.V.TIME)] = 0;
    raw.cc[@intFromEnum(std.posix.V.MIN)] = 1;

    try std.posix.tcsetattr(stdin.handle, .FLUSH, raw);
}

fn restoreTerminal(self: *LineEditor) !void {
    if (self.original_termios) |termios| {
        const stdin = std.fs.File.stdin();
        try std.posix.tcsetattr(stdin.handle, .FLUSH, termios);
        self.original_termios = null;
    }
    const stdout = std.fs.File.stdout();
    try stdout.writeAll("\r");
}

fn refreshLine(self: *LineEditor, prompt: []const u8) !void {
    const stdout = std.fs.File.stdout();

    // Clear the current line
    try stdout.writeAll(ESCAPE_CLEAR_LINE);

    // Reprint prompt and buffer
    try stdout.writeAll(prompt);
    try stdout.writeAll(self.buffer.items);

    // Move cursor to correct position
    const chars_after_cursor = self.buffer.items.len - self.cursor_pos;
    if (chars_after_cursor > 0) {
        var buf: [32]u8 = undefined;
        const escape_seq = try std.fmt.bufPrint(&buf, ESCAPE_CURSOR_LEFT, .{chars_after_cursor});
        try stdout.writeAll(escape_seq);
    }
}

fn moveCursorTo(self: *LineEditor, new_pos: usize, prompt: []const u8) !void {
    self.cursor_pos = new_pos;
    try self.refreshLine(prompt);
}
