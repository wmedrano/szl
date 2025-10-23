const std = @import("std");
const testing = std.testing;

const Diagnostics = @import("../Diagnostics.zig");
const Instruction = @import("../instruction.zig").Instruction;
const Module = @import("../types/Module.zig");
const NativeProc = @import("../types/NativeProc.zig");
const Handle = @import("../types/object_pool.zig").Handle;
const Proc = @import("../types/Proc.zig");
const Symbol = @import("../types/Symbol.zig");
const Val = @import("../types/Val.zig");
const Builder = @import("../utils/Builder.zig");
const Vm = @import("../Vm.zig");
const boolean_fns = @import("boolean_fns.zig");
const character_fns = @import("character_fns.zig");
const control_fns = @import("control_fns.zig");
const equivalence_fns = @import("equivalence_fns.zig");
const exception_fns = @import("exception_fns.zig");
const number_fns = @import("number_fns.zig");
const pair_fns = @import("pair_fns.zig");
const parameter_fns = @import("parameter_fns.zig");
const string_fns = @import("string_fns.zig");
const system_interface_fns = @import("system_interface_fns.zig");

pub const import = NativeProc{
    .name = "%szl-import",
    .unsafe_impl = &importImpl,
    .docstring = "Internal mechanism used to import.",
};

// TODO: This should support the full `import` syntax specified by r7rs. This
// includes things like renames and skipping certain values.
fn importImpl(vm: *Vm, diagnostics: ?*Diagnostics, arg_count: u32) Vm.Error!void {
    if (arg_count != 1) {
        if (diagnostics) |d| {
            d.addDiagnostic(.{ .wrong_arg_count = .{
                .expected = 1,
                .got = arg_count,
                .proc = Val.initNativeProc(&import),
            } });
        }
        return Vm.Error.UncaughtException;
    }
    const inspector = vm.inspector();
    const module_specifier = try inspector.listToSliceAlloc(vm.allocator(), vm.context.top() orelse
        return Vm.Error.UndefinedBehavior);
    defer vm.allocator().free(module_specifier);
    const module_symbols = try vm.allocator().alloc(Symbol, module_specifier.len);
    defer vm.allocator().free(module_symbols);
    for (module_specifier, module_symbols) |val, *sym| {
        sym.* = val.asSymbol() orelse {
            if (diagnostics) |d| {
                d.addDiagnostic(.{ .wrong_arg_type = .{
                    .expected = "symbol",
                    .got = val,
                    .proc = Val.initNativeProc(&import),
                    .arg_name = "module-specifier",
                    .arg_position = 0,
                } });
            }
            return Vm.Error.UncaughtException;
        };
    }
    // TODO: Import into the correct environment.
    const dst_module = try inspector.getReplEnv(null);
    const src_module = inspector.findModule(module_symbols) orelse {
        if (diagnostics) |d| {
            d.addDiagnostic(.{ .other = "Module not found" });
        }
        return Vm.Error.UncaughtException;
    };
    try (try inspector.handleToModule(dst_module)).import(
        vm.allocator(),
        (try inspector.handleToModule(src_module)).*,
    );
    try vm.context.push(vm.allocator(), Val.initEmptyList());
}

pub fn init(vm: *Vm) Vm.Error!Handle(Module) {
    const b = vm.builder();

    const env_handle = try b.makeEnvironment(&.{
        (try b.makeStaticSymbolHandle("scheme")),
        (try b.makeStaticSymbolHandle("base")),
    }, &[_]Builder.Definition{
        // 6.1 Equivalence
        .{ .symbol = (try b.makeStaticSymbolHandle("eq?")), .value = Val.initNativeProc(&equivalence_fns.eq_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("eqv")), .value = Val.initNativeProc(&equivalence_fns.eqv_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("equal?")), .value = Val.initNativeProc(&equivalence_fns.equal_p) },
        // 6.2 Numbers
        .{ .symbol = (try b.makeStaticSymbolHandle("+")), .value = Val.initNativeProc(&number_fns.add) },
        .{ .symbol = (try b.makeStaticSymbolHandle("-")), .value = Val.initNativeProc(&number_fns.sub) },
        .{ .symbol = (try b.makeStaticSymbolHandle("<")), .value = Val.initNativeProc(&number_fns.lt) },
        .{ .symbol = (try b.makeStaticSymbolHandle("<=")), .value = Val.initNativeProc(&number_fns.lte) },
        .{ .symbol = (try b.makeStaticSymbolHandle(">")), .value = Val.initNativeProc(&number_fns.gt) },
        .{ .symbol = (try b.makeStaticSymbolHandle(">=")), .value = Val.initNativeProc(&number_fns.gte) },
        .{ .symbol = (try b.makeStaticSymbolHandle("=")), .value = Val.initNativeProc(&number_fns.eq) },
        .{ .symbol = (try b.makeStaticSymbolHandle("number?")), .value = Val.initNativeProc(&number_fns.number_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("integer?")), .value = Val.initNativeProc(&number_fns.integer_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("exact-integer?")), .value = Val.initNativeProc(&number_fns.exact_integer_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("rational?")), .value = Val.initNativeProc(&number_fns.rational_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("exact?")), .value = Val.initNativeProc(&number_fns.exact_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("inexact?")), .value = Val.initNativeProc(&number_fns.inexact_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("round")), .value = Val.initNativeProc(&number_fns.round) },
        // 6.3 Booleans
        .{ .symbol = (try b.makeStaticSymbolHandle("not")), .value = Val.initNativeProc(&boolean_fns.not) },
        .{ .symbol = (try b.makeStaticSymbolHandle("boolean?")), .value = Val.initNativeProc(&boolean_fns.boolean_p) },
        // 6.4 Pairs and lists
        .{ .symbol = (try b.makeStaticSymbolHandle("pair?")), .value = Val.initNativeProc(&pair_fns.pair_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("cons")), .value = Val.initNativeProc(&pair_fns.cons) },
        .{ .symbol = (try b.makeStaticSymbolHandle("car")), .value = Val.initNativeProc(&pair_fns.car) },
        .{ .symbol = (try b.makeStaticSymbolHandle("cdr")), .value = Val.initNativeProc(&pair_fns.cdr) },
        .{ .symbol = (try b.makeStaticSymbolHandle("set-car!")), .value = Val.initNativeProc(&pair_fns.set_car_b) },
        .{ .symbol = (try b.makeStaticSymbolHandle("set-cdr!")), .value = Val.initNativeProc(&pair_fns.set_cdr_b) },
        .{ .symbol = (try b.makeStaticSymbolHandle("null?")), .value = Val.initNativeProc(&pair_fns.null_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("list?")), .value = Val.initNativeProc(&pair_fns.list_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("make-list")), .value = Val.initNativeProc(&pair_fns.make_list) },
        .{ .symbol = (try b.makeStaticSymbolHandle("list")), .value = Val.initNativeProc(&pair_fns.list) },
        .{ .symbol = (try b.makeStaticSymbolHandle("length")), .value = Val.initNativeProc(&pair_fns.length) },
        .{ .symbol = (try b.makeStaticSymbolHandle("append")), .value = Val.initNativeProc(&pair_fns.append) },
        .{ .symbol = (try b.makeStaticSymbolHandle("reverse")), .value = Val.initNativeProc(&pair_fns.reverse) },
        .{ .symbol = (try b.makeStaticSymbolHandle("list-tail")), .value = Val.initNativeProc(&pair_fns.list_tail) },
        .{ .symbol = (try b.makeStaticSymbolHandle("list-ref")), .value = Val.initNativeProc(&pair_fns.list_ref) },
        .{ .symbol = (try b.makeStaticSymbolHandle("list-set!")), .value = Val.initNativeProc(&pair_fns.list_set_b) },
        // 6.5 Symbols
        // 6.6 Characters
        .{ .symbol = (try b.makeStaticSymbolHandle("char?")), .value = Val.initNativeProc(&character_fns.char_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char=?")), .value = Val.initNativeProc(&character_fns.eq) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char<?")), .value = Val.initNativeProc(&character_fns.lt) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char<=?")), .value = Val.initNativeProc(&character_fns.lte) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char>?")), .value = Val.initNativeProc(&character_fns.gt) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char>=?")), .value = Val.initNativeProc(&character_fns.gte) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-ci=?")), .value = Val.initNativeProc(&character_fns.ci_eq) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-ci<?")), .value = Val.initNativeProc(&character_fns.ci_lt) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-ci<=?")), .value = Val.initNativeProc(&character_fns.ci_lte) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-ci>?")), .value = Val.initNativeProc(&character_fns.ci_gt) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-ci>=?")), .value = Val.initNativeProc(&character_fns.ci_gte) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-alphabetic?")), .value = Val.initNativeProc(&character_fns.char_alphabetic_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-numeric?")), .value = Val.initNativeProc(&character_fns.char_numeric_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-whitespace?")), .value = Val.initNativeProc(&character_fns.char_whitespace_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-upper-case?")), .value = Val.initNativeProc(&character_fns.char_upper_case_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-lower-case?")), .value = Val.initNativeProc(&character_fns.char_lower_case_p) },
        .{ .symbol = (try b.makeStaticSymbolHandle("digit-value")), .value = Val.initNativeProc(&character_fns.digit_value) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char->integer")), .value = Val.initNativeProc(&character_fns.char_to_integer) },
        .{ .symbol = (try b.makeStaticSymbolHandle("integer->char")), .value = Val.initNativeProc(&character_fns.integer_to_char) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-upcase")), .value = Val.initNativeProc(&character_fns.char_upcase) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-downcase")), .value = Val.initNativeProc(&character_fns.char_downcase) },
        .{ .symbol = (try b.makeStaticSymbolHandle("char-foldcase")), .value = Val.initNativeProc(&character_fns.char_foldcase) },
        // 6.7 Strings
        .{ .symbol = (try b.makeStaticSymbolHandle("string-length")), .value = Val.initNativeProc(&string_fns.string_length) },
        // 6.8 Vectors
        // 6.9 Bytevectors
        // 6.10 Control features
        .{ .symbol = (try b.makeStaticSymbolHandle("apply")), .value = Val.initNativeProc(&control_fns.apply) },
        .{ .symbol = (try b.makeStaticSymbolHandle("call/cc")), .value = Val.initNativeProc(&control_fns.call_cc) },
        .{ .symbol = (try b.makeStaticSymbolHandle("call-with-current-continuation")), .value = Val.initNativeProc(&control_fns.call_cc) },
        .{ .symbol = (try b.makeStaticSymbolHandle("make-parameter")), .value = Val.initNativeProc(&parameter_fns.make_parameter) },
        .{ .symbol = (try b.makeStaticSymbolHandle("%sizzle-set-parameter")), .value = Val.initNativeProc(&parameter_fns.set_parameter) },
        // 6.11 Environment and evaluation
        // 6.12 Exceptions
        .{ .symbol = (try b.makeStaticSymbolHandle("with-exception-handler")), .value = Val.initNativeProc(&exception_fns.with_exception_handler) },
        .{ .symbol = (try b.makeStaticSymbolHandle("raise-continuable")), .value = Val.initNativeProc(&exception_fns.raise_continuable) },
        .{ .symbol = (try b.makeStaticSymbolHandle("%szl-raise-next")), .value = Val.initNativeProc(&exception_fns.szl_raise_next) },
        // 6.13 Input and Output
        // 6.14 System interface
        .{ .symbol = (try b.makeStaticSymbolHandle("exit")), .value = Val.initNativeProc(&system_interface_fns.exit) },
        // 5.6 Libraries
        .{ .symbol = (try b.makeStaticSymbolHandle("%szl-import")), .value = Val.initNativeProc(&import) },
    });
    const inspector = vm.inspector();
    const env = try inspector.handleToModule(env_handle);

    // raise
    const raise_proc = Val.initProc(try vm.builder().makeProc(
        Proc{
            .instructions = try vm.allocator().dupe(Instruction, &[_]Instruction{
                // (raise-continuable err)
                Instruction{ .load_arg = 0 },
                Instruction{ .load_global = env.getSlot(try b.makeStaticSymbolHandle("raise-continuable")).? },
                Instruction{ .eval = 1 },
                // (%szl-raise-next err)
                Instruction{ .load_arg = 0 },
                Instruction{ .load_global = env.getSlot(try b.makeStaticSymbolHandle("%szl-raise-next")).? },
                Instruction{ .eval = 1 },
                Instruction{ .ret = {} },
            }),
            .constants = &.{},
            .module = env_handle,
            .name = try b.makeStaticSymbolHandle("raise"),
            .arg_count = 1,
            .locals_count = 0,
            .captures_count = 0,
        },
    ));
    try env.setBySymbol(vm.allocator(), try b.makeStaticSymbolHandle("raise"), raise_proc);

    // TODO: Support multiple arguments. This is a stub that supports only 1
    // argument for call-with-values.
    //
    // (call-with-values producer consumer)
    // (define (call-with-values producer consumer)
    //   (consumer (producer)))
    const call_with_values_proc = Val.initProc(try vm.builder().makeProc(
        Proc{
            .instructions = try vm.allocator().dupe(Instruction, &[_]Instruction{
                // (producer)
                Instruction{ .load_arg = 0 },
                Instruction{ .eval = 0 },
                // (consumer _)
                Instruction{ .load_arg = 1 },
                Instruction{ .eval = 1 },
                Instruction{ .ret = {} },
            }),
            .constants = &.{},
            .module = env_handle,
            .name = try b.makeStaticSymbolHandle("call-with-values-proc"),
            .arg_count = 1,
            .locals_count = 0,
            .captures_count = 0,
        },
    ));
    try env.setBySymbol(
        vm.allocator(),
        try b.makeStaticSymbolHandle("call-with-values-proc"),
        call_with_values_proc,
    );

    _ = try vm.evalStr(@embedFile("base.scm"), env_handle, null);
    return env_handle;
}

test "cond" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval(
        "greater",
        "(cond ((> 3 2) 'greater) ((< 3 2) 'less))",
    );
    try vm.expectEval(
        "equal",
        "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))",
    );
}

test "unless" {
    var vm = try Vm.init(.{ .allocator = testing.allocator });
    defer vm.deinit();
    try vm.expectEval("a", "(unless #f 'a)");
    try vm.expectEval("c", "(unless #f 'a 'b' 'c)");
    try vm.expectEval("()", "(unless #t 'a)");
    try vm.expectEval("()", "(unless #t 'a 'b 'c)");
}
