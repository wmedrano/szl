# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

szl is a Scheme interpreter implemented in Zig. The project compiles Scheme expressions to bytecode instructions, executes them on a stack-based virtual machine, and uses a mark-and-sweep garbage collector.

**Requirements:** Zig 0.15

**IMPORTANT:** This project does NOT use git. Never use git commands (git stash, git commit, git status, etc.).

**IMPORTANT:** When testing with the REPL (`zig-out/bin/szl`), Claude Code runs in script mode (via stdin), NOT interactive REPL mode. This means:
- The REPL will exit on the first error
- You cannot interact with the REPL interactively
- For testing, use `printf 'expression\n' | zig-out/bin/szl` to pipe script input
- Always prefer `zig build test --summary all` for testing - it's faster and more reliable

## Zig 0.15 Compatibility Notes

**IMPORTANT:** In Zig 0.15, `ArrayList` behaves like the old `ArrayListUnmanaged`:
- `ArrayList` does NOT store an allocator internally
- All `ArrayList` methods require passing an allocator explicitly (e.g., `list.append(allocator, item)`)
- Methods like `init()` do not take an allocator parameter: `ArrayList(T).init()`
- Methods like `deinit()` do not take an allocator parameter: `list.deinit(allocator)`
- If you need the old managed behavior, use `ArrayListAligned` instead

**Custom Formatting in Zig 0.15:**
- To enable custom formatting, implement a `format` method with this signature:
  ```zig
  pub fn format(self: YourType, writer: *std.Io.Writer) std.Io.Writer.Error!void {
      try writer.print("...", .{...});
      try writer.writeAll("...");
  }
  ```
- The `writer` parameter is `*std.Io.Writer` (defined in this codebase)
- Use `writer.print()`, `writer.writeAll()`, etc. to output formatted text
- Once implemented, you can use `{f}` format specifier: `std.debug.print("{f}", .{your_value});`

## Build Commands

### Build and Run
```bash
zig build                    # Build the project
zig-out/bin/szl             # Run the REPL
zig-out/bin/szl < file.scm  # Run a script
zig build run -- [args]     # Build and run with arguments
```

### Testing
```bash
zig build test --summary all  # Run all tests (VERY FAST: ~2s for full suite)
zig test src/Vm.zig          # Run tests for specific module
```

**Development Loop:** The test suite is extremely fast (~2 seconds for 241 tests). Always run `zig build test --summary all` first when making changes - it's faster than manual REPL testing and catches issues immediately. Only use the REPL for interactive exploration after tests pass.

### Documentation
```bash
zig build doc  # Generate documentation in zig-out/docs/
```

### Benchmarking
```bash
./scripts/benchmark_perf.sh       # Profile with perf
./scripts/benchmark_callgrind.sh  # Profile with callgrind
```

## Architecture

### Core Components

**VM (src/Vm.zig)**: Central VM that owns all objects and coordinates execution
- `Objects`: All heap-allocated objects (symbols, pairs, strings, modules, procs, closures, vectors, bytevectors, continuations, syntax_rules)
- `Context`: Execution context with stack and stack frames
- `init()`: Initializes VM and sets up standard libraries `(scheme base)` and `(user repl)` environment

**Context (src/Context.zig)**: Stack-based execution context
- `stack`: Value stack for computation
- `stack_frame`: Current stack frame with instruction pointer, args, locals, captures, exception handler
- `stack_frames`: Stack frame history for call/return

**Val (src/types/Val.zig)**: Tagged union representing all Scheme values
- Immediate: `empty_list`, `boolean`, `int`, `float`, `symbol`, `native_proc`
- Heap-allocated: `pair`, `string`, `module`, `proc`, `closure`, `vector`, `bytevector`, `continuation`, `syntax_rules`

### Compilation Pipeline

**Reader (src/compiler/Reader.zig)**: Reads S-expressions from source text
- Returns `Val` representation of parsed expressions

**Compiler (src/compiler/Compiler.zig)**: Compiles S-expressions to bytecode
- `Scope`: Tracks variables (args, locals, captures) and their locations
- Resolves variable references to arg/local/capture/module locations
- Generates `Instruction` sequences
- Handles closures by tracking captured variables

**Instruction (src/instruction.zig)**: Stack-based bytecode operations
- Stack manipulation: `push_const`, `squash`
- Variable access: `get_arg`, `get_local`, `get_capture`, `get_global`, `get_proc`
- Variable mutation: `set_local`, `set_global`
- Control flow: `jump`, `jump_if_not`, `ret`
- Function calls: `eval`, `make_closure`
- `executeUntilEnd()`: Main instruction execution loop

### Memory Management

**Gc (src/Gc.zig)**: Mark-and-sweep garbage collector
- Marks from roots: Context stack/frames, all Module objects
- Sweeps unmarked objects from object pools
- Modules are never collected (always roots)

**ObjectPool (src/types/object_pool.zig)**: Generic pool allocator for heap objects
- Provides `Handle<T>` references to pooled objects
- Used for all heap-allocated types

### Standard Library

**Base Library (src/schemelib/base.zig)**: Implements `(scheme base)` R7RS standard library
- Initialized as global environment on VM startup
- Imported by `(user repl)` environment

**Other Libraries**:
- `number_fns.zig`: Numeric operations
- `boolean_fns.zig`: Boolean operations
- `equivalence_fns.zig`: Equality predicates
- `sizzle_unstable_compiler.zig`: Compiler introspection functions

### Utilities

**Builder (src/utils/Builder.zig)**: Constructs Scheme values programmatically
- `makeList()`, `makeVector()`, `makeBytevector()`, `makeString()`, etc.

**Inspector (src/utils/Inspector.zig)**: Inspects and extracts data from Scheme values
- Type checking and conversions
- Access to REPL environment

**PrettyPrinter (src/utils/PrettyPrinter.zig)**: Formats values for display

## Testing Patterns

**IMPORTANT:** Always run `zig build test --summary all` FIRST when making changes. The test suite is extremely fast (~2s) and will catch compilation errors and test failures immediately. Manual REPL testing is slower and should only be used for interactive exploration after tests pass.

- Tests are inline using `test` blocks
- Use `Vm.expectEval(expected, source)` to test evaluation results
- The VM persists across evaluations in the REPL - reset with `Context.reset()` for isolated tests
- When adding new types to Val.Data union, ensure all switch statements are updated (compiler will catch missing cases)

## Key Design Notes

- The VM uses arena allocation for temporary compilation data
- Closures capture variables by index, resolved at compile time
- Stack frames maintain exception handlers for error propagation
- The REPL accumulates input until expressions are complete (handles multi-line input)
