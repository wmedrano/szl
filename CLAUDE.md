# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

szl is a Scheme interpreter library for Zig that implements a virtual machine-based execution model. The project provides a complete Scheme implementation including parsing, compilation to bytecode, and execution.

## Build Commands

- `zig build` - Build the library (default target)
- `zig build test` - Run all unit tests
- `zig build docs` - Generate documentation (output to zig-out/docs)

## Architecture

### Core Components

The interpreter follows a pipeline architecture:

1. **Tokenizer** (`src/Tokenizer.zig`) - Breaks source text into token spans, handling parentheses and symbols
2. **Reader** (`src/Reader.zig`) - Parses tokens into Scheme values using the VM for value creation
3. **Compiler** (`src/Compiler.zig`) - Transforms Scheme expressions into bytecode instructions
4. **VM** (`src/Vm.zig`) - Virtual machine that executes bytecode and manages runtime environment

### Value System

- **Val** (`src/Val.zig`) - Core value representation using tagged unions for dynamic typing
- Supports: nil, booleans, i64/f64 numbers, characters, symbols, pairs, procedures (bytecode), native procedures, strings, and vectors
- **Symbol** (`src/Symbol.zig`) - Interned symbols for efficient comparison
- **Pair** (`src/Pair.zig`) - Cons cells for list structures
- **Char** (`src/Char.zig`) - Character values with #\ notation
- **String** (`src/String.zig`) - Dynamic string values with escape sequence support
- **Vector** (`src/Vector.zig`) - Dynamic arrays of values with #(...) syntax

### Memory Management

- **ObjectPool** (`src/object_pool.zig`) - Handle-based object management for Pair, Procedure, String, and Vector types
- **Builder** (`src/Builder.zig`) - Value construction utilities that handle object pool allocation
- **Inspector** (`src/Inspector.zig`) - Type conversion and object pool resolution utilities
- All heap objects (strings, pairs, procedures, vectors) use handle-based references for memory safety

### Compilation System

- **LexicalScope** (`src/LexicalScope.zig`) - Variable binding management during compilation
- **Instruction** (`src/instruction.zig`) - Bytecode instruction definitions
- **Procedure** (`src/Procedure.zig`) - Bytecode procedure representation with direct instruction storage
- **Procedure.Native** - Native Zig function procedures for built-in operations

### Built-ins

Located in `src/builtins/`:
- `boolean.zig` - Boolean operations (boolean?, not)
- `define.zig` - Variable definition primitives
- `equivalence.zig` - Equivalence operations (eq?, equal?)
- `numbers.zig` - Numeric operations (+, -, *, /, <, >, =, <=, >=, abs)
- `builtins.zig` - Built-in function registry

### Debugging & Utilities

- **Inspector** (`src/Inspector.zig`) - Runtime introspection utilities
- **PrettyPrinter** (`src/PrettyPrinter.zig`) - Value formatting for display

## Testing Approach

The project uses Zig's built-in testing framework. Tests are embedded within modules using `test` blocks. Key testing patterns:

- `vm.expectEval(expected, expression)` - Test expression evaluation
- `vm.expectReadOne(expected_tag, expected_format, input)` - Test token parsing with type validation
- Integration tests in `src/root.zig` demonstrate language features
- Unit tests in individual modules test specific functionality
- Memory management tests ensure proper cleanup and no double-free errors

## Language Features

Currently implemented Scheme features:
- Basic data types (numbers, booleans, symbols, characters, strings, lists, vectors)
- Special forms: `define`, `quote`, `if`, `let`, `begin`, `cond`
- Arithmetic and comparison operations (+, -, *, /, <, >, =, <=, >=, abs)
- Boolean operations (boolean?, not)
- Equivalence operations (eq?, equal?)
- Lexical scoping with proper variable binding
- Bytecode compilation and execution
- Vector syntax: `#(element1 element2 ...)` with support for nested vectors and mixed types

## Common Development Patterns

When adding new data types (like vectors, strings):
1. Create the data structure module (e.g., `Vector.zig`)
2. Add the type variant to `Val.Repr` union in `Val.zig`
3. Add object pool to `Vm.zig` if the type needs heap allocation
4. Update `Builder.zig` to support construction of the new type
5. Update `Inspector.zig` for type conversion and resolution
6. Update `PrettyPrinter.zig` for display formatting
7. Update `Compiler.zig` if the type needs compilation support
8. Update tokenizer/reader for new syntax (if applicable)
9. Add comprehensive tests including edge cases and memory management

When adding new syntax:
1. Update `Tokenizer.zig` for token recognition
2. Update `Reader.zig` for parsing logic
3. Update `Compiler.zig` for compilation support
4. Add tests for all parsing scenarios

When adding built-in functions:
1. Add to appropriate file in `src/builtins/`
2. Update `CommonSymbolTable` in `Vm.zig` for new special forms
3. Add comprehensive tests using the `expectEval` pattern