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
- Supports: nil, booleans, i64/f64 numbers, characters, symbols, pairs, and procedures
- **Symbol** (`src/Symbol.zig`) - Interned symbols for efficient comparison
- **Pair** (`src/Pair.zig`) - Cons cells for list structures
- **Char** (`src/Char.zig`) - Character values with #\ notation

### Memory Management

- **ObjectPool** (`src/object_pool.zig`) - Handle-based object management
- **Builder** (`src/Builder.zig`) - Value construction utilities
- All heap objects use handle-based references for memory safety

### Compilation System

- **LexicalScope** (`src/LexicalScope.zig`) - Variable binding management during compilation
- **Instruction** (`src/instruction.zig`) - Bytecode instruction definitions
- **Procedure** (`src/Procedure.zig`) - Compiled procedure representation

### Built-ins

Located in `src/builtins/`:
- `arithmetic.zig` - Basic math operations (+, -, *, /)
- `comparison.zig` - Comparison operations (<, >, =, etc.)
- `math.zig` - Advanced math functions
- `define.zig` - Variable definition primitives
- `builtins.zig` - Built-in function registry

### Debugging & Utilities

- **Inspector** (`src/Inspector.zig`) - Runtime introspection utilities
- **PrettyPrinter** (`src/PrettyPrinter.zig`) - Value formatting for display

## Testing Approach

The project uses Zig's built-in testing framework. Tests are embedded within modules using `test` blocks. Key testing patterns:

- `vm.expectEval(expected, expression)` - Test expression evaluation
- Integration tests in `src/root.zig` demonstrate language features
- Unit tests in individual modules test specific functionality

## Language Features

Currently implemented Scheme features:
- Basic data types (numbers, booleans, symbols, characters, lists)
- Special forms: `define`, `quote`, `if`, `let`, `begin`, `cond`
- Arithmetic and comparison operations
- Lexical scoping with proper variable binding
- Bytecode compilation and execution

## Common Development Patterns

When adding new functionality:
1. Add value types to `Val.zig` if needed
2. Extend the compiler in `Compiler.zig` for new syntax
3. Add built-in functions to appropriate files in `src/builtins/`
4. Update `CommonSymbolTable` in `Vm.zig` for new special forms
5. Add comprehensive tests using the `expectEval` pattern