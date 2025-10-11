const std = @import("std");

const Vm = @import("../Vm.zig");
const Handle = @import("object_pool.zig").Handle;
const Module = @import("Module.zig");
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");

const SyntaxRules = @This();

/// Symbols that must match literally (not treated as pattern variables)
literals: std.ArrayList(Symbol),
/// List of pattern/template transformation rules
rules: std.ArrayList(Rule),
/// Environment where the macro was defined (for hygiene)
defining_env: Handle(Module),

pub const Rule = struct {
    pattern: Pattern,
    template: Template,

    pub fn deinit(self: *Rule, allocator: std.mem.Allocator) void {
        self.pattern.deinit(allocator);
        self.template.deinit(allocator);
    }
};

pub const Pattern = union(enum) {
    /// Match this exact symbol (from literals list)
    literal: Symbol,
    /// Match anything and bind to this pattern variable
    variable: Symbol,
    /// Match a list of sub-patterns
    list: std.ArrayList(Pattern),
    /// Match zero or more repetitions of a pattern
    ellipsis: struct {
        pattern: *Pattern,
    },
    /// Match underscore wildcard (matches anything, no binding)
    any,

    pub fn deinit(self: *Pattern, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .list => |*lst| {
                for (lst.items) |*item| {
                    item.deinit(allocator);
                }
                lst.deinit(allocator);
            },
            .ellipsis => |e| {
                e.pattern.deinit(allocator);
                allocator.destroy(e.pattern);
            },
            .literal, .variable, .any => {},
        }
    }
};

pub const Template = union(enum) {
    /// Insert this literal value
    literal: Val,
    /// Substitute a pattern variable's bound value
    variable: Symbol,
    /// Construct a list from sub-templates
    list: std.ArrayList(Template),
    /// Repeat template for each binding of ellipsis pattern variable
    ellipsis: struct {
        template: *Template,
    },

    pub fn deinit(self: *Template, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .list => |*lst| {
                for (lst.items) |*item| {
                    item.deinit(allocator);
                }
                lst.deinit(allocator);
            },
            .ellipsis => |e| {
                e.template.deinit(allocator);
                allocator.destroy(e.template);
            },
            .literal, .variable => {},
        }
    }
};

/// Stores bindings from pattern matching
pub const Bindings = struct {
    // Map from pattern variable symbol to bound value(s)
    // For non-ellipsis patterns: stores single value
    // For ellipsis patterns: stores list of values
    map: std.AutoHashMap(Symbol, BindingValue),

    pub const BindingValue = union(enum) {
        single: Val,
        multiple: std.ArrayList(Val),

        pub fn deinit(self: *BindingValue, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .multiple => |*lst| lst.deinit(allocator),
                .single => {},
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator) Bindings {
        return Bindings{
            .map = std.AutoHashMap(Symbol, BindingValue).init(allocator),
        };
    }

    pub fn deinit(self: *Bindings, allocator: std.mem.Allocator) void {
        var iter = self.map.valueIterator();
        while (iter.next()) |val| {
            val.deinit(allocator);
        }
        self.map.deinit();
    }
};

pub fn deinit(self: *SyntaxRules, allocator: std.mem.Allocator) void {
    self.literals.deinit(allocator);
    for (self.rules.items) |*rule| {
        rule.deinit(allocator);
    }
    self.rules.deinit(allocator);
}

/// Attempt to match a usage form against this macro's rules and expand
pub fn expand(self: SyntaxRules, vm: *Vm, usage: Val) Vm.Error!?Val {
    const inspector = vm.inspector();

    // Usage form should be a list (macro-name arg1 arg2 ...)
    // We match against (pattern arg1 arg2 ...) where pattern is from rules
    const usage_list = inspector.listToSliceAlloc(vm.allocator(), usage) catch {
        return null; // Not a proper list, can't match
    };
    defer vm.allocator().free(usage_list);

    if (usage_list.len == 0) return null;

    // Try each rule in order
    for (self.rules.items) |*rule| {
        var bindings = Bindings.init(vm.allocator());
        defer bindings.deinit(vm.allocator());

        // Match against the whole usage form (including macro name)
        if (try self.matchPattern(rule.pattern, usage, &bindings, vm)) {
            return try self.expandTemplate(rule.template, bindings, vm);
        }
    }

    return null; // No rule matched
}

/// Match a pattern against an expression, populating bindings
fn matchPattern(
    self: SyntaxRules,
    pattern: Pattern,
    expr: Val,
    bindings: *Bindings,
    vm: *Vm,
) Vm.Error!bool {
    switch (pattern) {
        .any => return true,
        .literal => |lit_sym| {
            const expr_sym = expr.asSymbol() orelse return false;
            return lit_sym.eq(expr_sym);
        },

        .variable => |var_sym| {
            // Bind this variable to the expression
            try bindings.map.put(var_sym, .{ .single = expr });
            return true;
        },

        .list => |patterns| {
            // Expression must be a list of the same structure
            const inspector = vm.inspector();
            const expr_list = inspector.listToSliceAlloc(vm.allocator(), expr) catch {
                return false; // Not a proper list
            };
            defer vm.allocator().free(expr_list);

            // Check for ellipsis patterns
            var pattern_idx: usize = 0;
            var expr_idx: usize = 0;

            while (pattern_idx < patterns.items.len) {
                const pat = patterns.items[pattern_idx];

                // Check if next pattern is ellipsis
                if (pat == .ellipsis) {
                    const ellipsis_pattern = pat.ellipsis.pattern;

                    // Collect all expressions that match the ellipsis pattern
                    var matches = std.ArrayList(Val){};
                    defer matches.deinit(vm.allocator());

                    // Match as many as possible
                    while (expr_idx < expr_list.len) {
                        // Try to match remaining fixed patterns first
                        const remaining_patterns = patterns.items.len - pattern_idx - 1;
                        const remaining_exprs = expr_list.len - expr_idx;
                        if (remaining_exprs == remaining_patterns) {
                            // Need to stop ellipsis matching to match remaining patterns
                            break;
                        }

                        var temp_bindings = Bindings.init(vm.allocator());
                        defer temp_bindings.deinit(vm.allocator());

                        if (try self.matchPattern(ellipsis_pattern.*, expr_list[expr_idx], &temp_bindings, vm)) {
                            try matches.append(vm.allocator(), expr_list[expr_idx]);

                            // Merge ellipsis bindings
                            try self.mergeEllipsisBindings(bindings, temp_bindings, vm.allocator());
                            expr_idx += 1;
                        } else {
                            break;
                        }
                    }

                    pattern_idx += 1;
                    continue;
                }

                // Regular pattern matching
                if (expr_idx >= expr_list.len) return false;

                if (!try self.matchPattern(pat, expr_list[expr_idx], bindings, vm)) {
                    return false;
                }

                pattern_idx += 1;
                expr_idx += 1;
            }

            // All patterns must be consumed
            return expr_idx == expr_list.len;
        },

        .ellipsis => {
            // Ellipsis patterns should only appear inside lists
            return Vm.Error.InvalidExpression;
        },
    }
}

/// Merge bindings from ellipsis pattern matching into multiple bindings
fn mergeEllipsisBindings(
    self: SyntaxRules,
    dest: *Bindings,
    src: Bindings,
    allocator: std.mem.Allocator,
) !void {
    _ = self;
    var iter = src.map.iterator();
    while (iter.next()) |entry| {
        const sym = entry.key_ptr.*;
        const val = entry.value_ptr.*;

        if (dest.map.getPtr(sym)) |existing| {
            // Append to existing multiple binding
            switch (existing.*) {
                .single => |single_val| {
                    // Convert to multiple
                    var list = std.ArrayList(Val){};
                    try list.append(allocator, single_val);
                    switch (val) {
                        .single => |v| try list.append(allocator, v),
                        .multiple => |m| try list.appendSlice(allocator, m.items),
                    }
                    existing.* = .{ .multiple = list };
                },
                .multiple => |*existing_list| {
                    switch (val) {
                        .single => |v| try existing_list.append(allocator, v),
                        .multiple => |m| try existing_list.appendSlice(allocator, m.items),
                    }
                },
            }
        } else {
            // Create new multiple binding
            var list = std.ArrayList(Val){};
            switch (val) {
                .single => |v| try list.append(allocator, v),
                .multiple => |m| try list.appendSlice(allocator, m.items),
            }
            try dest.map.put(sym, .{ .multiple = list });
        }
    }
}

/// Expand a template using the bindings from pattern matching
fn expandTemplate(
    self: SyntaxRules,
    template: Template,
    bindings: Bindings,
    vm: *Vm,
) Vm.Error!Val {
    const builder = vm.builder();

    switch (template) {
        .literal => |val| return val,

        .variable => |var_sym| {
            const binding = bindings.map.get(var_sym) orelse {
                // If there's no binding, treat it as a literal symbol
                return Val.initSymbol(var_sym);
            };
            switch (binding) {
                .single => |val| return val,
                .multiple => {
                    // Shouldn't reference ellipsis binding outside ellipsis template
                    return Vm.Error.InvalidExpression;
                },
            }
        },

        .list => |templates| {
            var results = std.ArrayList(Val){};
            defer results.deinit(vm.allocator());

            for (templates.items) |tmpl| {
                if (tmpl == .ellipsis) {
                    // Expand ellipsis template multiple times
                    const ellipsis_tmpl = tmpl.ellipsis.template.*;

                    // Find the first pattern variable to determine repetition count
                    const rep_count = try self.getEllipsisRepetitionCount(ellipsis_tmpl, bindings);

                    var i: usize = 0;
                    while (i < rep_count) : (i += 1) {
                        // Create temporary bindings with indexed values
                        var temp_bindings = Bindings.init(vm.allocator());
                        defer temp_bindings.deinit(vm.allocator());

                        try self.extractEllipsisBindings(&temp_bindings, bindings, i);

                        const expanded = try self.expandTemplate(ellipsis_tmpl, temp_bindings, vm);
                        try results.append(vm.allocator(), expanded);
                    }
                } else {
                    const expanded = try self.expandTemplate(tmpl, bindings, vm);
                    try results.append(vm.allocator(), expanded);
                }
            }

            return try builder.makeList(results.items);
        },

        .ellipsis => {
            // Ellipsis templates should only appear inside lists
            return Vm.Error.InvalidExpression;
        },
    }
}

/// Get the number of times an ellipsis template should be repeated
fn getEllipsisRepetitionCount(
    self: SyntaxRules,
    template: Template,
    bindings: Bindings,
) !usize {
    switch (template) {
        .variable => |var_sym| {
            if (bindings.map.get(var_sym)) |binding| {
                switch (binding) {
                    .multiple => |list| return list.items.len,
                    .single => return 1,
                }
            }
            return 0;
        },
        .list => |templates| {
            // Return the maximum count from any variable
            var max_count: usize = 0;
            for (templates.items) |tmpl| {
                const count = try self.getEllipsisRepetitionCount(tmpl, bindings);
                if (count > max_count) max_count = count;
            }
            return max_count;
        },
        .ellipsis => |e| {
            return try self.getEllipsisRepetitionCount(e.template.*, bindings);
        },
        .literal => return 0,
    }
}

/// Extract the i-th element from ellipsis bindings
fn extractEllipsisBindings(
    self: SyntaxRules,
    dest: *Bindings,
    src: Bindings,
    index: usize,
) !void {
    _ = self;
    var iter = src.map.iterator();
    while (iter.next()) |entry| {
        const sym = entry.key_ptr.*;
        const val = entry.value_ptr.*;

        switch (val) {
            .single => |v| {
                // Single values are reused for each repetition
                try dest.map.put(sym, .{ .single = v });
            },
            .multiple => |list| {
                if (index < list.items.len) {
                    try dest.map.put(sym, .{ .single = list.items[index] });
                }
            },
        }
    }
}
