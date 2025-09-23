//! Record implementation for the Scheme interpreter.
//!
//! This module provides the record data structure used in R7RS Scheme. Records are
//! user-defined data types created with define-record-type. Each record has a type
//! identifier and a fixed number of fields that can be accessed and modified.
//! Records support inheritance and type checking at runtime.

const std = @import("std");
const testing = std.testing;

const Handle = @import("../object_pool.zig").Handle;
const Symbol = @import("Symbol.zig");
const Val = @import("Val.zig");
const Vm = @import("../Vm.zig");

const Record = @This();

/// The record type descriptor containing type information.
/// This identifies what kind of record this is and provides type metadata.
type_descriptor_handle: Handle(RecordTypeDescriptor),

/// Direct reference to the type descriptor for faster access.
/// This avoids the need to look up the handle in the object pool for common operations.
type_descriptor: RecordTypeDescriptor,

/// The field values stored in this record instance.
/// The number and meaning of fields is determined by the type descriptor.
fields: []Val,

/// Record type descriptor that defines the structure and behavior of a record type.
/// This contains all the metadata needed to work with records of this type.
pub const RecordTypeDescriptor = struct {
    /// The name/symbol that identifies this record type.
    /// Used for type checking and display purposes.
    name: Symbol.Interned,

    /// The names of the fields in this record type.
    /// Determines the order and meaning of field values.
    field_names: []const Symbol.Interned,

    /// Creates a new record type descriptor.
    ///
    /// Args:
    ///   allocator: The memory allocator to use for the field names list.
    ///   name: The symbol identifying this record type.
    ///   field_names: The field names for this record type.
    ///
    /// Returns:
    ///   A new RecordTypeDescriptor instance.
    ///
    /// Errors:
    ///   Returns OutOfMemory if allocation fails.
    pub fn init(allocator: std.mem.Allocator, name: Symbol.Interned, field_names: []const Symbol.Interned) !RecordTypeDescriptor {
        const field_names_copy = try allocator.dupe(Symbol.Interned, field_names);
        return RecordTypeDescriptor{
            .name = name,
            .field_names = field_names_copy,
        };
    }

    /// Releases all memory used by the record type descriptor.
    ///
    /// Args:
    ///   self: The record type descriptor to deinitialize.
    ///   allocator: The allocator used to create the descriptor.
    pub fn deinit(self: *RecordTypeDescriptor, allocator: std.mem.Allocator) void {
        allocator.free(self.field_names);
    }

    /// Returns the number of fields in this record type.
    ///
    /// Args:
    ///   self: The record type descriptor to query.
    ///
    /// Returns:
    ///   The number of fields in this record type.
    pub fn fieldCount(self: RecordTypeDescriptor) usize {
        return self.field_names.len;
    }

    /// Gets the field name at the specified index.
    ///
    /// Args:
    ///   self: The record type descriptor to query.
    ///   index: The index of the field name to retrieve.
    ///
    /// Returns:
    ///   The field name at the specified index, or error if index is out of bounds.
    ///
    /// Errors:
    ///   Returns IndexOutOfBounds if the index is invalid.
    pub fn getFieldName(self: RecordTypeDescriptor, index: usize) !Symbol.Interned {
        if (index >= self.field_names.len) {
            return error.IndexOutOfBounds;
        }
        return self.field_names[index];
    }

    /// Finds the index of a field by name.
    ///
    /// Args:
    ///   self: The record type descriptor to search.
    ///   field_name: The field name to search for.
    ///
    /// Returns:
    ///   The index of the field, or error if not found.
    ///
    /// Errors:
    ///   Returns FieldNotFound if the field name is not in this record type.
    pub fn findFieldIndex(self: RecordTypeDescriptor, field_name: Symbol.Interned) !usize {
        for (self.field_names, 0..) |name, i| {
            if (name.eql(field_name)) {
                return i;
            }
        }
        return error.FieldNotFound;
    }
};

/// Creates a new record with the given type descriptor.
///
/// The record will be initialized with nil values for all fields.
///
/// Args:
///   allocator: The memory allocator to use for the record's field storage.
///   vm: The virtual machine instance containing the type descriptor.
///   type_descriptor_handle: Handle to the type descriptor defining this record's structure.
///
/// Returns:
///   A new Record instance with nil values for all fields.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
pub fn init(allocator: std.mem.Allocator, vm: *const Vm, type_descriptor_handle: Handle(RecordTypeDescriptor)) !Record {
    const type_descriptor = vm.record_type_descriptors.get(type_descriptor_handle) orelse return error.InvalidTypeDescriptor;
    const fields = try allocator.alloc(Val, type_descriptor.fieldCount());

    // Initialize all fields to nil
    for (fields) |*field| {
        field.* = Val.init({});
    }

    return Record{
        .type_descriptor_handle = type_descriptor_handle,
        .type_descriptor = type_descriptor,
        .fields = fields,
    };
}

/// Creates a new record with the given type descriptor and initial field values.
///
/// Args:
///   allocator: The memory allocator to use for the record's field storage.
///   vm: The virtual machine instance containing the type descriptor.
///   type_descriptor_handle: Handle to the type descriptor defining this record's structure.
///   field_values: The initial values for the record's fields.
///
/// Returns:
///   A new Record instance with the specified field values.
///
/// Errors:
///   Returns OutOfMemory if allocation fails.
///   Returns FieldCountMismatch if field_values length doesn't match type descriptor.
pub fn initWithValues(allocator: std.mem.Allocator, vm: *const Vm, type_descriptor_handle: Handle(RecordTypeDescriptor), field_values: []const Val) !Record {
    const type_descriptor = vm.record_type_descriptors.get(type_descriptor_handle) orelse return error.InvalidTypeDescriptor;
    if (field_values.len != type_descriptor.fieldCount()) {
        return error.FieldCountMismatch;
    }
    const fields = try allocator.dupe(Val, field_values);
    return Record{
        .type_descriptor_handle = type_descriptor_handle,
        .type_descriptor = type_descriptor,
        .fields = fields,
    };
}

/// Releases all memory used by the record.
///
/// After calling this method, the record should not be used.
/// Note: This does not deinitialize the type descriptor.
///
/// Args:
///   self: The record to deinitialize.
///   allocator: The allocator used to create the record.
pub fn deinit(self: *Record, allocator: std.mem.Allocator) void {
    allocator.free(self.fields);
}

/// Gets the value of the field at the specified index.
///
/// Args:
///   self: The record to access.
///   index: The index of the field to retrieve.
///
/// Returns:
///   The value of the field at the specified index, or error if index is out of bounds.
///
/// Errors:
///   Returns IndexOutOfBounds if the index is invalid.
pub fn getField(self: Record, index: usize) !Val {
    if (index >= self.fields.len) return error.IndexOutOfBounds;
    return self.fields[index];
}

/// Sets the value of the field at the specified index.
///
/// Args:
///   self: The record to modify.
///   index: The index of the field to set.
///   value: The new value to store in the field.
///
/// Errors:
///   Returns IndexOutOfBounds if the index is invalid.
pub fn setField(self: *Record, index: usize, value: Val) !void {
    if (index >= self.fields.len) return error.IndexOutOfBounds;
    self.fields[index] = value;
}

/// Gets the value of the field with the specified name.
///
/// Args:
///   self: The record to access.
///   vm: The virtual machine instance containing the type descriptor.
///   field_name: The name of the field to retrieve.
///
/// Returns:
///   The value of the named field, or error if field is not found.
///
/// Errors:
///   Returns FieldNotFound if the field name is not in this record type.
pub fn getFieldByName(self: Record, field_name: Symbol.Interned) !Val {
    const index = try self.type_descriptor.findFieldIndex(field_name);
    return try self.getField(index);
}

/// Sets the value of the field with the specified name.
///
/// Args:
///   self: The record to modify.
///   vm: The virtual machine instance containing the type descriptor.
///   field_name: The name of the field to set.
///   value: The new value to store in the field.
///
/// Errors:
///   Returns FieldNotFound if the field name is not in this record type.
pub fn setFieldByName(self: *Record, field_name: Symbol.Interned, value: Val) !void {
    const index = try self.type_descriptor.findFieldIndex(field_name);
    try self.setField(index, value);
}

/// Returns the number of fields in this record.
///
/// Args:
///   self: The record to query.
///
/// Returns:
///   The number of fields in the record.
pub fn fieldCount(self: Record) usize {
    return self.type_descriptor.fieldCount();
}

/// Checks if this record is of the specified type.
///
/// Args:
///   self: The record to check.
///   vm: The virtual machine instance containing the type descriptor.
///   type_name: The type name to check against.
///
/// Returns:
///   true if the record is of the specified type, false otherwise.
pub fn isOfType(self: Record, type_name: Symbol.Interned) bool {
    return self.type_descriptor.name.eql(type_name);
}

test "Record is small" {
    // We can reduce the size by moving the `RecordTypeDescriptor` handle into
    // the `Val` object.
    try testing.expectEqual(48, @sizeOf(Record));
    try testing.expectEqual(24, @sizeOf(RecordTypeDescriptor));
}

test "RecordTypeDescriptor init creates empty descriptor" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const name = try vm.interner.internStatic(Symbol.init("person"));
    const field_names = [_]Symbol.Interned{};
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const resolved_descriptor = vm.record_type_descriptors.get(descriptor_handle).?;
    try testing.expectEqual(name, resolved_descriptor.name);
    try testing.expectEqual(@as(usize, 0), resolved_descriptor.fieldCount());
}

test "RecordTypeDescriptor init adds fields correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const last_name = try vm.interner.internStatic(Symbol.init("last-name"));

    const field_names = [_]Symbol.Interned{ first_name, last_name };
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const final_descriptor = vm.record_type_descriptors.get(descriptor_handle).?;
    try testing.expectEqual(2, final_descriptor.fieldCount());
    try testing.expect((try final_descriptor.getFieldName(0)).eql(first_name));
    try testing.expect((try final_descriptor.getFieldName(1)).eql(last_name));
}

test "RecordTypeDescriptor findFieldIndex finds correct indices" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const age = try vm.interner.internStatic(Symbol.init("age"));

    const field_names = [_]Symbol.Interned{ first_name, age };
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const final_descriptor = vm.record_type_descriptors.get(descriptor_handle).?;
    try testing.expectEqual(@as(usize, 0), try final_descriptor.findFieldIndex(first_name));
    try testing.expectEqual(@as(usize, 1), try final_descriptor.findFieldIndex(age));

    const unknown = try vm.interner.internStatic(Symbol.init("unknown"));
    try testing.expectError(error.FieldNotFound, final_descriptor.findFieldIndex(unknown));
}

test "Record init creates record with nil fields" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const field_names = [_]Symbol.Interned{first_name};
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    var record = try Record.init(testing.allocator, &vm, descriptor_handle);
    defer record.deinit(testing.allocator);

    try testing.expectEqual(@as(usize, 1), record.fieldCount());
    const field_value = try record.getField(0);
    try testing.expect(field_value.isNil());
}

test "Record initWithValues creates record with specified values" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const age = try vm.interner.internStatic(Symbol.init("age"));

    const field_names = [_]Symbol.Interned{ first_name, age };
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    const values = [_]Val{ Val.init(42), Val.init(true) };
    var record = try Record.initWithValues(testing.allocator, &vm, descriptor_handle, &values);
    defer record.deinit(testing.allocator);

    try testing.expectEqual(2, record.fieldCount());
    try testing.expectEqual(Val.init(42), try record.getField(0));
    try testing.expectEqual(Val.init(true), try record.getField(1));
}

test "Record getField and setField work correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const field_names = [_]Symbol.Interned{first_name};
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    var record = try Record.init(testing.allocator, &vm, descriptor_handle);
    defer record.deinit(testing.allocator);

    try record.setField(0, Val.init(42));
    try testing.expectEqual(Val.init(42), try record.getField(0));

    try testing.expectError(error.IndexOutOfBounds, record.getField(1));
    try testing.expectError(error.IndexOutOfBounds, record.setField(1, Val.init(0)));
}

test "Record getFieldByName and setFieldByName work correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const type_name = try vm.interner.internStatic(Symbol.init("person"));
    const first_name = try vm.interner.internStatic(Symbol.init("first-name"));
    const age = try vm.interner.internStatic(Symbol.init("age"));
    const field_names = [_]Symbol.Interned{ first_name, age };
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, type_name, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    var record = try Record.init(testing.allocator, &vm, descriptor_handle);
    defer record.deinit(testing.allocator);

    try record.setFieldByName(first_name, Val.init(42));
    try record.setFieldByName(age, Val.init(25));

    try testing.expectEqual(Val.init(42), try record.getFieldByName(first_name));
    try testing.expectEqual(Val.init(25), try record.getFieldByName(age));

    const unknown = try vm.interner.internStatic(Symbol.init("unknown"));
    try testing.expectError(error.FieldNotFound, record.getFieldByName(unknown));
    try testing.expectError(error.FieldNotFound, record.setFieldByName(unknown, Val.init(0)));
}

test "Record isOfType works correctly" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();

    const person_type = try vm.interner.internStatic(Symbol.init("person"));
    const car_type = try vm.interner.internStatic(Symbol.init("car"));

    const field_names = [_]Symbol.Interned{};
    const descriptor = try RecordTypeDescriptor.init(testing.allocator, person_type, &field_names);
    const descriptor_handle = try vm.builder().buildHandle(descriptor);

    var record = try Record.init(testing.allocator, &vm, descriptor_handle);
    defer record.deinit(testing.allocator);

    try testing.expect(record.isOfType(person_type));
    try testing.expect(!record.isOfType(car_type));
}
