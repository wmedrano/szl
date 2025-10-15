const std = @import("std");

const Context = @import("Context.zig");
const Instruction = @import("instruction.zig").Instruction;
const Continuation = @import("types/Continuation.zig");
const Module = @import("types/Module.zig");
const Handle = @import("types/object_pool.zig").Handle;
const Pair = @import("types/Pair.zig");
const Proc = @import("types/Proc.zig");
const Record = @import("types/Record.zig");
const String = @import("types/String.zig");
const SyntaxRules = @import("types/SyntaxRules.zig");
const Val = @import("types/Val.zig");
const Vector = Val.Vector;
const ByteVector = Val.ByteVector;
const Inspector = @import("utils/Inspector.zig");
const Vm = @import("Vm.zig");

const Gc = @This();

arena: *std.heap.ArenaAllocator,
marked: std.AutoHashMapUnmanaged(GcObject, void) = .{},

const GcObject = union(enum) {
    proc: Handle(Proc),
    pair: Handle(Pair),
    string: Handle(String),
    vector: Handle(Vector),
    bytevector: Handle(ByteVector),
    continuation: Handle(Continuation),
    syntax_rules: Handle(SyntaxRules),
    record: Handle(Record),
    record_descriptor: Handle(Record.Descriptor),

    fn init(val: Val) ?GcObject {
        return switch (val.data) {
            .empty_list, .boolean, .int, .float, .char, .symbol, .native_proc => null,
            // Modules are on the heap, but we don't clean them up.
            .module => null,
            .proc => |p| .{ .proc = p },
            .pair => |p| .{ .pair = p },
            .string => |s| .{ .string = s },
            .vector => |v| .{ .vector = v },
            .bytevector => |bv| .{ .bytevector = bv },
            .continuation => |c| .{ .continuation = c },
            .syntax_rules => |sr| .{ .syntax_rules = sr },
            .record => |r| .{ .record = r },
            .record_descriptor => |rd| .{ .record_descriptor = rd },
        };
    }
};

pub fn markOne(self: *Gc, vm: *Vm, val: Val) Vm.Error!void {
    const obj = GcObject.init(val) orelse return;
    const res = try self.marked.getOrPut(self.arena.allocator(), obj);
    if (res.found_existing) return;
    switch (obj) {
        .proc => |h| {
            const proc = try vm.inspector().handleToProc(h);
            try self.markMany(vm, proc.constants);
        },
        .pair => |h| {
            const pair = try vm.inspector().handleToPair(h);
            try self.markOne(vm, pair.car);
            try self.markOne(vm, pair.cdr);
        },
        .string => {},
        .vector => |h| {
            const vec = try vm.inspector().handleToVector(h);
            try self.markMany(vm, vec.items);
        },
        .bytevector => {},
        .continuation => |h| {
            const continuation = try vm.inspector().handleToContinuation(h);
            try self.markContext(vm, continuation.context);
        },
        .syntax_rules => {},
        .record => |h| {
            const record = try vm.inspector().handleToRecord(h);
            try self.markOne(vm, Val{ .data = .{ .record_descriptor = record.descriptor } });
            for (record.fields) |field| {
                try self.markOne(vm, field);
            }
        },
        .record_descriptor => {},
    }
}

fn markMany(self: *Gc, vm: *Vm, vals: []const Val) Vm.Error!void {
    for (vals) |v| try self.markOne(vm, v);
}

pub fn markModule(self: *Gc, vm: *Vm, h: Handle(Module)) Vm.Error!void {
    const module = try vm.inspector().handleToModule(h);
    for (module.slots.items) |v| try self.markOne(vm, v);
}

pub fn markContext(self: *Gc, vm: *Vm, context: Context) Vm.Error!void {
    try self.markMany(vm, context.stack.items);
    for (context.stack_frames.items) |sf| try self.markStackFrame(vm, sf);
}

fn markStackFrame(self: *Gc, vm: *Vm, stack_frame: Context.StackFrame) Vm.Error!void {
    try self.markOne(vm, stack_frame.proc);
    try self.markOne(vm, stack_frame.exception_handler);
    try self.markMany(vm, stack_frame.constants);
}

pub fn sweep(self: Gc, vm: *Vm) Vm.Error!usize {
    var removed_count: usize = 0;

    const RemovePair = struct {
        gc: Gc,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(Pair), _: *Pair) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .pair = h });
            if (should_remove) this.count.* += 1;
            return should_remove;
        }
    };
    try vm.objects.pairs.removeAll(
        vm.allocator(),
        RemovePair{ .gc = self, .count = &removed_count },
    );

    const RemoveString = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(String), obj: *String) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .string = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.strings.removeAll(
        vm.allocator(),
        RemoveString{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    const RemoveProc = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(Proc), obj: *Proc) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .proc = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.procs.removeAll(
        vm.allocator(),
        RemoveProc{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    const RemoveVector = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(Vector), obj: *Vector) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .vector = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.vectors.removeAll(
        vm.allocator(),
        RemoveVector{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    const RemoveBytevector = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(ByteVector), obj: *ByteVector) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .bytevector = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.bytevectors.removeAll(
        vm.allocator(),
        RemoveBytevector{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    const RemoveContinuation = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(Continuation), obj: *Continuation) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .continuation = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.continuations.removeAll(
        vm.allocator(),
        RemoveContinuation{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    const RemoveSyntaxRules = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(SyntaxRules), obj: *SyntaxRules) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .syntax_rules = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.syntax_rules.removeAll(
        vm.allocator(),
        RemoveSyntaxRules{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    const RemoveRecord = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(Record), obj: *Record) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .record = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.records.removeAll(
        vm.allocator(),
        RemoveRecord{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    const RemoveRecordDescriptor = struct {
        gc: Gc,
        allocator: std.mem.Allocator,
        count: *usize,
        pub fn remove(this: @This(), h: Handle(Record.Descriptor), obj: *Record.Descriptor) bool {
            const should_remove = !this.gc.marked.contains(GcObject{ .record_descriptor = h });
            if (should_remove) {
                obj.deinit(this.allocator);
                this.count.* += 1;
            }
            return should_remove;
        }
    };
    try vm.objects.record_descriptors.removeAll(
        vm.allocator(),
        RemoveRecordDescriptor{ .gc = self, .allocator = vm.allocator(), .count = &removed_count },
    );

    return removed_count;
}
