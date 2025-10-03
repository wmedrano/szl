const std = @import("std");

const Symbol = @This();

string: []const u8,

pub fn init(str: []const u8) Symbol {
    return Symbol{ .string = str };
}
