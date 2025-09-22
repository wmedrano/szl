# szl

A Scheme interpreter library for Zig.

## Features

- **Complete Scheme Implementation**: Parsing, compilation to bytecode, and execution
- **Virtual Machine Architecture**: Efficient bytecode execution with lexical scoping
- **Memory Safe**: Handle-based object management for automatic memory safety
- **Rich Data Types**: Numbers, booleans, symbols, characters, strings, lists, and vectors
- **Built-in Functions**: Comprehensive arithmetic, comparison, and utility operations

## Quick Start

### Building

```bash
# Build the library
zig build

# Run tests
zig build test

# Generate documentation
zig build docs
```

### Basic Usage

```zig
const std = @import("std");
const szl = @import("szl");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var vm = szl.Vm.init(gpa.allocator());
    defer vm.deinit();

    // Evaluate Scheme expressions
    const result = try vm.eval("(+ 1 2 3)");
    // result is 6
}
```

## Language Support

### Data Types
- **Numbers**: Integers (`42`) and floating-point (`3.14`)
- **Booleans**: `#t` and `#f`
- **Characters**: `#\a`, `#\space`, `#\newline`
- **Strings**: `"hello world"` with escape sequences
- **Symbols**: `foo`, `bar`, `+`
- **Lists**: `(1 2 3)`, `'(a b c)`
- **Vectors**: `#(1 2 3)`, `#(a "hello" #t)`

### Special Forms
- `define` - Variable and function definition
- `quote` / `'` - Prevent evaluation
- `if` - Conditional expressions
- `let` and `let*` - Local variable binding
- `begin` - Sequential evaluation
- `cond` - Multi-way conditionals

### Built-in Functions
- **Arithmetic**: `+`, `-`, `*`, `/`
- **Comparison**: `=`, `<`, `>`, `<=`, `>=`
- **Type predicates**: `number?`, `string?`, `symbol?`, etc.

## Examples

```scheme
;; Define variables
(define x 42)
(define name "Alice")

;; Define functions
(define square (lambda (n) (* n n)))
(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

;; Use vectors
(define v #(1 2 3))

;; Conditional logic
(define sign
  (lambda (x)
    (cond
      ((> x 0) 'positive)
      ((< x 0) 'negative)
      (else 'zero))))
```

## Architecture

The interpreter uses a four-stage pipeline:

1. **Tokenizer** - Lexical analysis
2. **Reader** - Parse tokens into Scheme values
3. **Compiler** - Transform to bytecode instructions
4. **VM** - Execute bytecode with runtime environment

## Development

szl is designed as a library for embedding Scheme interpretation in Zig
applications. The codebase follows Zig conventions and uses the built-in testing
framework.

### Project Structure
- `src/` - Core implementation
- `src/builtins/` - Built-in function implementations
- `build.zig` - Build configuration

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file
for details.
