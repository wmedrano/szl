# szl

A Scheme interpreter implemented in Zig.

## Quickstart

### REPL Basics

Sizzle requires Zig 0.15. To start the interactive REPL:

```bash
zig build -Doptimize=ReleaseFast
zig-out/bin/szl
```

Type expressions and press Enter to evaluate them. Use `(exit)` or Ctrl+D to quit.

### Syntax: S-Expressions

Sizzle uses prefix notation with parentheses. Instead of `f(x, y)`, write `(f x y)`.

```scheme
; 2 + 3 * 4 becomes:
(+ 2 (* 3 4))  ; => 14

; Chained operations
(+ 1 2 3 4 5)  ; => 15
(* 2 3 4)      ; => 24
```

### Variables and Functions

Use `define` to create variables and functions:

```scheme
; Variable
(define x 42)

; Function
(define (square n)
  (* n n))

(square 5)  ; => 25

; Anonymous functions with lambda
(define double (lambda (n) (* n 2)))
(double 7)  ; => 14
```

### Local Bindings

Use `let` to create temporary variables:

```scheme
(let ((x 5)
      (y 10))
  (+ x y))  ; => 15
```

In `let`, bindings happen simultaneously, so they can't reference each other. Use `let*` for sequential bindings:

```scheme
; let* allows each binding to use previous ones
(let* ((x 5)
       (y (* x 2)))  ; y can reference x
  (+ x y))  ; => 15
```

### Data Types

```scheme
; Numbers
42          ; integer
3.14        ; float
1/2         ; rational
(+ 1 2.5)   ; => 3.5
(+ 1/2 1/4) ; => 3/4
(+ 1/2 1/2) ; => 1

; Booleans
#t          ; true
#f          ; false

; Strings
"hello"
(string-append "hello" " " "world")  ; => "hello world"

; Lists
'(1 2 3)                    ; literal list
(list 1 2 3)                ; constructed list
(cons 1 '(2 3))             ; prepend: => (1 2 3)
(car '(1 2 3))              ; first element: => 1
(cdr '(1 2 3))              ; rest: => (2 3)

; Vectors (indexed arrays)
#(1 2 3)                    ; literal vector
(vector 1 2 3)              ; constructed
(vector-ref #(10 20 30) 1)  ; => 20
```

### Control Flow

```scheme
; If expression
(if (> 5 3)
    "yes"
    "no")  ; => "yes"

; Cond for multiple branches
(define (grade score)
  (cond
    ((>= score 90) "A")
    ((>= score 80) "B")
    ((>= score 70) "C")
    (else "F")))

(grade 85)  ; => "B"

; And/or (short-circuit evaluation)
(and #t #f)      ; => #f
(or #f #t)       ; => #t
```

### Working with Lists

```scheme
; Map
(map
 (lambda (x) (* x 2))
 '(1 2 3))  ; => (2 4 6)

; Filter
(filter
 (lambda (x) (> x 5))
 '(3 7 2 9))  ; => (7 9)

; Fold/reduce
(fold-left
 + 0
 '(1 2 3 4))  ; => 10

; Length
(length '(1 2 3))  ; => 3

; Check empty
(null? '())   ; => #t
(null? '(1))  ; => #f
```


### Try It Out

Here's a complete example to try in the REPL:

```scheme
; Define a function to find the maximum in a list
(define (max-list lst)
  (if (null? (cdr lst))
      (car lst)
      (let ((rest-max (max-list (cdr lst))))
        (if (> (car lst) rest-max)
            (car lst)
            rest-max))))

(max-list '(3 7 2 9 1))  ; => 9

; Define a function using higher-order functions
(define (compose f g)
  (lambda (x) (f (g x))))

(define add1 (lambda (x) (+ x 1)))
(define double (lambda (x) (* x 2)))
(define add1-then-double (compose double add1))

(add1-then-double 5)  ; => 12
```

## Building

Build the project using Zig's build system. Sizzle requires Zig 0.15.

```bash
zig build -Doptimize=ReleaseFast
zig-out/bin/szl
```

### Testing

Run the test suite:

```bash
zig build test --summary all
```

### Documentation

- Generate API documentation under `zig-out/docs`: `zig build doc`
- Build the website under `zig-out/site`: `scripts/build_website.sh`
