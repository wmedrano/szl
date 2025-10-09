# szl

A Scheme interpreter implemented in Zig.

## Overview

szl is a Scheme language implementation that provides a virtual machine for executing Scheme programs. The interpreter includes:

- **VM**: A virtual machine for executing Scheme bytecode
- **Compiler**: Compiles Scheme expressions into executable instructions
- **Reader**: Parses Scheme source code
- **Types**: Support for Scheme data types including pairs, symbols, vectors, procedures, and continuations
- **Object Pool**: Memory management for Scheme objects

## Building

Build the project using Zig's build system:

```bash
zig build
```

## Running

The interpreter reads Scheme code from stdin and evaluates each expression:

```bash
zig build run < examples/fib.scm
```

### Example: Fibonacci

Run the Fibonacci example that calculates fib(30):

```bash
zig build run < examples/fib.scm
```

This will execute the recursive Fibonacci function defined in `examples/fib.scm`.

## Testing

Run the test suite:

```bash
zig build test
```

## Documentation

Generate API documentation:

```bash
zig build doc
```

The documentation will be available in `zig-out/docs/`.

## Project Structure

- `src/main.zig` - Entry point for the interpreter
- `src/Vm.zig` - Virtual machine implementation
- `src/compiler/` - Compiler and reader components
- `src/types/` - Scheme type implementations
- `src/instruction.zig` - VM instruction definitions
- `examples/` - Example Scheme programs
