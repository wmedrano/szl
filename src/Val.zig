const std = @import("std");

const Cons = @import("Cons.zig");

const Val = @This();

data: Data,

pub const Data = union(enum) {
    empty_list,
    int: i64,
    pair: *Cons,
};

pub fn format(self: Val, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (self.data) {
        .empty_list => try writer.writeAll("()"),
        .int => |n| try writer.print("{}", .{n}),
        .pair => |pair| {
            try writer.writeAll("(");
            try pair.car.format(writer);
            var current = pair.cdr;
            while (true) {
                switch (current.data) {
                    .empty_list => break,
                    .pair => |next_pair| {
                        try writer.writeAll(" ");
                        try next_pair.car.format(writer);
                        current = next_pair.cdr;
                    },
                    else => {
                        try writer.writeAll(" . ");
                        try current.format(writer);
                        break;
                    },
                }
            }
            try writer.writeAll(")");
        },
    }
}
