const std = @import("std");

const ErrorDetails = @import("../types/ErrorDetails.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Record = @import("../types/Record.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Vm = @import("../Vm.zig");

pub inline fn make_record_descriptor_impl(vm: *Vm, diagnostics: *ErrorDetails, args: []Val) Vm.Error!Val {
    if (args.len == 0) {
        @branchHint(.cold);
        diagnostics.addDiagnostic(vm.allocator(), .{ .other = "Expected at least 1 argument (name)" });
        return Vm.Error.UncaughtException;
    }

    // First argument is the record name
    const name_val = args[0];
    const record_name = name_val.asSymbol() orelse {
        @branchHint(.cold);
        diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
            .expected = "symbol",
            .got = name_val,
            .proc = Val.initNativeProc(&make_record_descriptor),
            .arg_name = "name",
            .arg_position = 0,
        } });
        return Vm.Error.UncaughtException;
    };

    // Remaining arguments are field names
    const field_count = args.len - 1;
    const field_names = try vm.allocator().alloc(Symbol, field_count);
    errdefer vm.allocator().free(field_names);

    var has_err = false;
    for (args[1..], 0..) |field_val, i| {
        const field_sym = field_val.asSymbol() orelse blk: {
            @branchHint(.cold);
            has_err = true;
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                .expected = "symbol",
                .got = field_val,
                .proc = Val.initNativeProc(&make_record_descriptor),
                .arg_name = "field-name",
                .arg_position = @intCast(i + 1),
            } });
            break :blk Symbol.init("");
        };
        field_names[i] = field_sym;
    }
    if (has_err) return Vm.Error.UncaughtException;

    const descriptor_handle = try vm.builder().makeRecordDescriptor(.{
        .name = record_name,
        .field_names = field_names,
    });

    return Val.initRecordDescriptor(descriptor_handle);
}

pub const make_record_descriptor = NativeProc.withRawArgs(struct {
    pub const name = "%sizzle-make-record-descriptor";
    pub const docstring = "Create a record descriptor with a name and field names.\n" ++
        "Usage: (%sizzle-make-record-descriptor name field-0 field-1 ...)\n" ++
        "All arguments must be symbols.";

    pub fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []Val) Vm.Error!Val {
        return make_record_descriptor_impl(vm, diagnostics, args);
    }
});

pub const record_get = NativeProc.with3Args(struct {
    pub const name = "%sizzle-record-get";
    pub const docstring = "Get a record field value by slot index.\n" ++
        "Usage: (%sizzle-record-get record-descriptor record slot-idx)\n" ++
        "Returns the value at the specified slot.";

    pub fn impl(vm: *Vm, diagnostics: *ErrorDetails, descriptor_val: Val, record_val: Val, slot_idx_val: Val) Vm.Error!Val {
        const inspector = vm.inspector();

        // Get the record descriptor
        const descriptor_handle = switch (descriptor_val.data) {
            .record_descriptor => |h| h,
            else => {
                @branchHint(.cold);
                diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "record-descriptor",
                    .got = descriptor_val,
                    .proc = Val.initNativeProc(&record_get),
                    .arg_name = "record-descriptor",
                    .arg_position = 0,
                } });
                return Vm.Error.UncaughtException;
            },
        };

        // Get the record
        const record = inspector.asRecord(record_val) catch {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                .expected = "record",
                .got = record_val,
                .proc = Val.initNativeProc(&record_get),
                .arg_name = "record",
                .arg_position = 1,
            } });
            return Vm.Error.UncaughtException;
        };

        // Validate that the record matches the descriptor
        if (!record.descriptor.eq(descriptor_handle)) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .other = "Record type mismatch: record does not match the provided descriptor" });
            return Vm.Error.UncaughtException;
        }

        // Get the slot index
        const slot_idx = slot_idx_val.asInt() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                .expected = "integer",
                .got = slot_idx_val,
                .proc = Val.initNativeProc(&record_get),
                .arg_name = "slot-idx",
                .arg_position = 2,
            } });
            return Vm.Error.UncaughtException;
        };

        if (slot_idx < 0) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .other = "Slot index must be non-negative" });
            return Vm.Error.UncaughtException;
        }

        // Get the field
        return try record.getField(vm, @intCast(slot_idx));
    }
});

pub const record_set_b = NativeProc.with4Args(struct {
    pub const name = "%sizzle-record-set!";
    pub const docstring = "Set a record field value by slot index.\n" ++
        "Usage: (%sizzle-record-set! record-descriptor record slot-idx val)\n" ++
        "Returns #<unspecified>.";

    pub fn impl(vm: *Vm, diagnostics: *ErrorDetails, descriptor_val: Val, record_val: Val, slot_idx_val: Val, new_val: Val) Vm.Error!Val {
        const inspector = vm.inspector();

        // Get the record descriptor
        const descriptor_handle = switch (descriptor_val.data) {
            .record_descriptor => |h| h,
            else => {
                @branchHint(.cold);
                diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "record-descriptor",
                    .got = descriptor_val,
                    .proc = Val.initNativeProc(&record_set_b),
                    .arg_name = "record-descriptor",
                    .arg_position = 0,
                } });
                return Vm.Error.UncaughtException;
            },
        };

        // Get the record
        const record = inspector.asRecord(record_val) catch {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                .expected = "record",
                .got = record_val,
                .proc = Val.initNativeProc(&record_set_b),
                .arg_name = "record",
                .arg_position = 1,
            } });
            return Vm.Error.UncaughtException;
        };

        // Validate that the record matches the descriptor
        if (!record.descriptor.eq(descriptor_handle)) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .other = "Record type mismatch: record does not match the provided descriptor" });
            return Vm.Error.UncaughtException;
        }

        // Get the slot index
        const slot_idx = slot_idx_val.asInt() orelse {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                .expected = "integer",
                .got = slot_idx_val,
                .proc = Val.initNativeProc(&record_set_b),
                .arg_name = "slot-idx",
                .arg_position = 2,
            } });
            return Vm.Error.UncaughtException;
        };

        if (slot_idx < 0) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .other = "Slot index must be non-negative" });
            return Vm.Error.UncaughtException;
        }

        // Set the field
        try record.setField(vm, @intCast(slot_idx), new_val);

        return Val.initUnspecified();
    }
});

pub const make_record = NativeProc.withRawArgs(struct {
    pub const name = "%sizzle-make-record";
    pub const docstring = "Create a record instance with the given descriptor and field values.\n" ++
        "Usage: (%sizzle-make-record descriptor field-val-0 field-val-1 ...)\n" ++
        "The number of field values must match the descriptor's field count.";

    pub fn impl(vm: *Vm, diagnostics: *ErrorDetails, args: []Val) Vm.Error!Val {
        if (args.len == 0) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .other = "Expected at least 1 argument (descriptor)" });
            return Vm.Error.UncaughtException;
        }

        // First argument is the record descriptor
        const descriptor_val = args[0];
        const descriptor_handle = switch (descriptor_val.data) {
            .record_descriptor => |h| h,
            else => {
                @branchHint(.cold);
                diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "record-descriptor",
                    .got = descriptor_val,
                    .proc = Val.initNativeProc(&make_record),
                    .arg_name = "descriptor",
                    .arg_position = 0,
                } });
                return Vm.Error.UncaughtException;
            },
        };

        const inspector = vm.inspector();
        const descriptor = try inspector.handleToRecordDescriptor(descriptor_handle);

        // Validate field count
        const field_values = args[1..];
        if (field_values.len != descriptor.field_names.len) {
            @branchHint(.cold);
            diagnostics.addDiagnostic(vm.allocator(), .{ .other = "Field count mismatch: expected fields for all descriptor fields" });
            return Vm.Error.UncaughtException;
        }

        // Create record with field values
        const fields = try vm.allocator().dupe(Val, field_values);
        errdefer vm.allocator().free(fields);

        const record = Record{
            .descriptor = descriptor_handle,
            .fields = fields,
        };

        const record_handle = try vm.objects.records.put(vm.allocator(), record);
        return Val{ .data = .{ .record = record_handle } };
    }
});

pub const record_p = NativeProc.with2Args(struct {
    pub const name = "%sizzle-record?";
    pub const docstring = "Check if a value is a record instance of a given descriptor.\n" ++
        "Usage: (%sizzle-record? descriptor obj)\n" ++
        "Returns #t if obj is a record matching the descriptor, #f otherwise.";

    pub fn impl(vm: *Vm, diagnostics: *ErrorDetails, descriptor_val: Val, obj_val: Val) Vm.Error!Val {
        // Get the record descriptor
        const descriptor_handle = switch (descriptor_val.data) {
            .record_descriptor => |h| h,
            else => {
                @branchHint(.cold);
                diagnostics.addDiagnostic(vm.allocator(), .{ .wrong_arg_type = .{
                    .expected = "record-descriptor",
                    .got = descriptor_val,
                    .proc = Val.initNativeProc(&record_p),
                    .arg_name = "descriptor",
                    .arg_position = 0,
                } });
                return Vm.Error.UncaughtException;
            },
        };

        // Check if obj is a record
        const inspector = vm.inspector();
        const record = inspector.asRecord(obj_val) catch {
            // Not a record at all
            return Val.initBool(false);
        };

        // Check if the record matches the descriptor
        return Val.initBool(record.descriptor.eq(descriptor_handle));
    }
});
