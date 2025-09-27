# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build the library
zig build

# Run all tests
zig build test

# Generate documentation
zig build docs
```

## Testing Guidelines

**IMPORTANT**: Always use `zig build test` to run tests, never `zig test <file>`. Individual file testing with `zig test` will fail due to import path dependencies in this project structure.

## Architecture Overview

szl is a Scheme interpreter library implemented in Zig with a four-stage execution pipeline:

1. **Tokenizer** (`src/compiler/Tokenizer.zig`) - Lexical analysis of Scheme source code
2. **Reader** (`src/compiler/Reader.zig`) - Parse tokens into Scheme values
3. **Compiler** (`src/compiler/Compiler.zig`) - Transform Scheme expressions into bytecode instructions
4. **VM** (`src/Vm.zig`) - Execute bytecode with runtime environment and garbage collection

### Key Components

- **VM (`src/Vm.zig`)** - Central virtual machine managing execution environment, symbol interning, object pool, and procedure call frames
- **Val (`src/types/Val.zig`)** - Core value representation system for all Scheme data types
- **Object Pool (`src/object_pool.zig`)** - Handle-based memory management with garbage collection for automatic memory safety
- **Builder (`src/Builder.zig`)** - Value construction and validation utilities
- **Instruction (`src/instruction.zig`)** - Bytecode instruction definitions

### Type System

Located in `src/types/`:
- **Val.zig** - Unified value representation
- **Pair.zig** - Cons cells for lists
- **Vector.zig** - Array-like collections
- **String.zig** - Immutable string values
- **Symbol.zig** - Interned symbols with fast comparison
- **Char.zig** - Character values
- **Record.zig** - User-defined types

### Built-in Functions

Located in `src/builtins/` and registered via `builtins/builtins.zig`:
- **numbers.zig** - Arithmetic operations (+, -, *, /)
- **boolean.zig** - Boolean predicates and operations
- **equivalence.zig** - Equality and comparison operators (=, <, >)
- **define.zig** - Variable and function definition
- **pair.zig** - List manipulation functions

New built-ins are registered by adding them to the `register` function in `src/builtins/builtins.zig`.

### Compilation Process

The compiler (`src/compiler/Compiler.zig`) uses lexical scoping (`src/compiler/Scope.zig`) to track variable bindings and generates bytecode instructions that the VM executes. Special forms like `define`, `if`, `let`, and `lambda` receive special compilation treatment.

### Memory Management

Uses handle-based object references through the object pool system, enabling automatic garbage collection while maintaining memory safety. All Scheme values are represented as handles that can be safely passed around without manual memory management.

## Development Workflow

### Adding New Built-in Functions

1. Create a new file in `src/builtins/` following existing patterns
2. Implement the native function following the `NativeProc` interface
3. Add registration call to `src/builtins/builtins.zig`
4. Add tests within the same file using `zig build test`

### Adding New Value Types

1. Create the type implementation in `src/types/`
2. Update `Val.zig` to include the new type in the union
3. Update related functions in `Builder.zig`, `Inspector.zig`, and `PrettyPrinter.zig`
4. Add appropriate built-in predicates and operations

### Library Structure

The main public API is exposed through `src/root.zig`, which exports:
- `Val` - The core value type system
- `Vm` - The virtual machine for executing Scheme code

For embedding szl in applications, typically only these two exports are needed.