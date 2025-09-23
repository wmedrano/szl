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

Located in `src/builtins/` and registered via `builtins.zig`:
- **numbers.zig** - Arithmetic operations (+, -, *, /)
- **boolean.zig** - Boolean predicates and operations
- **equivalence.zig** - Equality and comparison operators (=, <, >)
- **define.zig** - Variable and function definition
- **pair.zig** - List manipulation functions

### Compilation Process

The compiler (`src/compiler/Compiler.zig`) uses lexical scoping (`LexicalScope.zig`) to track variable bindings and generates bytecode instructions that the VM executes. Special forms like `define`, `if`, `let`, and `lambda` receive special compilation treatment.

### Memory Management

Uses handle-based object references through the object pool system, enabling automatic garbage collection while maintaining memory safety. All Scheme values are represented as handles that can be safely passed around without manual memory management.