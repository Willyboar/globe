# Globe IL Documentation

Globe IL is a simple intermediate language designed for compiling to the Erlang BEAM virtual machine. It provides a minimal SSA-based representation suitable for functional and concurrent programming.

## Table of Contents

1. [Basic Concepts](#1-basic-concepts)
   - 1.1 [Modules](#11-modules)
   - 1.2 [Functions](#12-functions)
   - 1.3 [SSA Form](#13-ssa-form)
2. [Types](#2-types)
3. [Values](#3-values)
4. [Instructions](#4-instructions)
   - 4.1 [Assignment](#41-assignment)
   - 4.2 [Constants](#42-constants)
   - 4.3 [Arithmetic](#43-arithmetic)
   - 4.4 [Comparisons](#44-comparisons)
   - 4.5 [Function Calls](#45-function-calls)
   - 4.6 [External Calls](#46-external-calls)
   - 4.7 [Control Flow](#47-control-flow)
   - 4.8 [Data Structures](#48-data-structures)
   - 4.9 [Concurrency](#49-concurrency)
5. [Complete Examples](#5-complete-examples)

---

## 1. Basic Concepts

### 1.1 Modules

Every Globe IL program is organized into modules. A module declaration begins with the `module` keyword followed by an identifier prefixed with `@`:

```
module @module_name
```

Module names must begin with `@` and contain only lowercase letters, numbers, and underscores.

### 1.2 Functions

Functions are the primary unit of code organization. A function definition has the following syntax:

```
[export] function @name(@param1: type1, @param2: type2) -> return_type {
  @label:
  instruction1
  instruction2
  ...
  ret @value
}
```

- **export**: Optional keyword to make the function visible to external callers
- **@name**: Function identifier starting with `@`
- **parameters**: Comma-separated list of `@name: type` pairs
- **return_type**: Type of the value returned by the function
- **labels**: Control flow targets prefixed with `@` and ending with `:`
- **ret**: Return statement that terminates the function

**Example:**
```
export function @add(@a: int, @b: int) -> int {
  @start:
  @result = add @a @b
  ret @result
}
```

### 1.3 SSA Form

Globe IL uses Static Single Assignment (SSA) form, where each variable is assigned exactly once. This simplifies analysis and optimization.

**Valid SSA:**
```
@x = const 10
@y = const 20
@sum = add @x @y
```

**Invalid (multiple assignments to same variable):**
```
@x = const 10
@x = const 20  // Error: @x already defined
```

---

## 2. Types

Globe IL supports the following types:

| Type | Description | Example Values |
|------|-------------|----------------|
| `int` | Integer numbers | `42`, `-17`, `0` |
| `float` | Floating-point numbers | `3.14`, `-0.5`, `1.0` |
| `string` | Text strings | `"hello"`, `"world"` |
| `atom` | Symbolic constants | `ok`, `error`, `true` |
| `bool` | Boolean values | `true`, `false` |
| `binary` | Byte sequences | Bitstrings, binaries |
| `tuple` | Fixed-size collections | `{1, 2, 3}` |
| `list` | Variable-size collections | `[1, 2, 3]` |
| `pid` | Process identifiers | Result of `spawn` |

---

## 3. Values

Values in Globe IL can be:

### 3.1 Variables
Variables are identifiers prefixed with `@`:
```
@x
@my_variable
@result_123
```

### 3.2 Integer Literals
```
@x = const 42
@negative = const -17
@zero = const 0
```

### 3.3 Float Literals
```
@pi = const 3.14159
@half = const 0.5
```

### 3.4 String Literals
```
@greeting = const "Hello, World!"
@format = const "Value: ~p~n"
```

Special characters can be used:
- `\n` - newline
- `\t` - tab
- `\"` - quote
- `\\` - backslash

### 3.5 Boolean Literals
```
@t = const true
@f = const false
```

### 3.6 Atom Literals
```
@status = const atom ok
@result = const atom error
```

---

## 4. Instructions

### 4.1 Assignment

The basic assignment instruction stores the result of an expression:

```
@variable = expression
```

**Example:**
```
@x = const 42
@y = add @x @x
```

### 4.2 Constants

Load a constant value:

```
@var = const value
```

**Examples:**
```
@num = const 42
@msg = const "hello"
@flag = const true
@status = const atom ok
```

### 4.3 Arithmetic

Binary arithmetic operations:

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Addition | `add @a @b` | a + b |
| Subtraction | `sub @a @b` | a - b |
| Multiplication | `mul @a @b` | a * b |
| Division | `div @a @b` | a / b (integer division) |

**Example:**
```
@a = const 10
@b = const 20
@sum = add @a @b        // 30
@diff = sub @b @a       // 10
@product = mul @a @b    // 200
@quotient = div @b @a   // 2
```

### 4.4 Comparisons

Compare two values and produce a boolean result:

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Equal | `cmp eq @a @b` | a == b |
| Less than | `cmp lt @a @b` | a < b |
| Less or equal | `cmp lte @a @b` | a <= b |
| Greater than | `cmp gt @a @b` | a > b |
| Greater or equal | `cmp gte @a @b` | a >= b |

**Example:**
```
@x = const 10
@y = const 20
@is_equal = cmp eq @x @y     // false
@is_less = cmp lt @x @y      // true
@is_greater = cmp gt @x @y   // false
```

### 4.5 Function Calls

**Call with assignment:**
```
@result = call @function_name(@arg1, @arg2)
```

**Call as statement (void call):**
```
call @function_name(@arg1, @arg2)
```

**Tail call (optimized recursion):**
```
tail_call @function_name(@arg1, @arg2)
```

**Example:**
```
function @factorial(@n: int) -> int {
  @start:
  @zero = const 0
  @is_zero = cmp eq @n @zero
  jump_if @is_zero @base @recurse

  @base:
  @one = const 1
  ret @one

  @recurse:
  @one_r = const 1
  @n_minus_1 = sub @n @one_r
  tail_call @factorial(@n_minus_1)
}
```

### 4.6 External Calls

Call functions from Erlang modules:

```
@result = external module_name function_name(@arg1, @arg2)
```

**Examples:**
```
// Print to console
@msg = const "Hello~n"
@ok = external io format(@msg)

// Get list length
@items = list 1 2 3
@len = external erlang length(@items)

// Format with arguments
@format = const "X=~p Y=~p~n"
@args = list 10 20
@ok = external io format(@format, @args)
```

### 4.7 Control Flow

**Labels:**
Define control flow targets:
```
@label_name:
```

**Unconditional jump:**
```
jump @label
```

**Conditional jump:**
```
jump_if @condition @true_label @false_label
```

**Return:**
```
ret @value
```

**Example:**
```
function @max(@a: int, @b: int) -> int {
  @start:
  @is_greater = cmp gt @a @b
  jump_if @is_greater @return_a @return_b

  @return_a:
  ret @a

  @return_b:
  ret @b
}
```

**Pattern matching:**
```
match @value {
  pattern1: @label1,
  pattern2: @label2,
  _: @default
}
```

Patterns can be:
- `_` - wildcard (matches anything)
- `@var` - variable binding
- literals - `42`, `"hello"`, `true`
- `{@a, @b}` - tuple pattern

**Example:**
```
@result = const atom ok
match @result {
  atom ok: @success,
  atom error: @failure,
  _: @default
}
```

### 4.8 Data Structures

**Lists:**
```
@items = list @elem1 @elem2 @elem3
@empty = list
```

**Tuples:**
```
@pair = tuple 2 @first @second
@triple = tuple 3 @a @b @c
```

**Tuple access:**
```
@first = tuple_get @pair 0
@second = tuple_get @pair 1
```

**Bitstrings:**
```
@bits = bitstring_build [@value1 8, @value2 16, @value3 8]
```

Where each element is `value size_in_bits`.

**Binary operations:**
```
@size = binary_size @bin
@slice = binary_slice @bin @position @length
@concat = binary_concat @bin1 @bin2
@modified = binary_set @bin @position @value
```

### 4.9 Concurrency

**Spawn a process:**
```
@pid = spawn @function_name(@arg1, @arg2)
```

**Send a message:**
```
send @pid @message
```

**Receive messages:**
```
receive {
  pattern1: @label1,
  pattern2: @label2,
  after @timeout: @timeout_label
}
```

**Example:**
```
function @worker() -> int {
  @start:
  receive {
    atom stop: @done,
    @msg: @process
  }

  @process:
  // Handle message
  jump @start

  @done:
  @zero = const 0
  ret @zero
}

function @main() -> int {
  @start:
  @pid = spawn @worker()
  @msg = const atom hello
  send @pid @msg
  @zero = const 0
  ret @zero
}
```

---

## 5. Complete Examples

### 5.1 Hello World

```
module @hello

export function @main() -> int {
  @start:
  @msg = const "Hello, World!~n"
  @ok = external io format(@msg)
  @zero = const 0
  ret @zero
}
```

### 5.2 Factorial (Recursive)

```
module @factorial

export function @factorial(@n: int) -> int {
  @start:
  @zero = const 0
  @is_zero = cmp eq @n @zero
  jump_if @is_zero @base_case @recursive_case

  @base_case:
  @one = const 1
  ret @one

  @recursive_case:
  @one_r = const 1
  @n_minus_1 = sub @n @one_r
  @sub_result = call @factorial(@n_minus_1)
  @result = mul @n @sub_result
  ret @result
}

export function @main() -> int {
  @start:
  @five = const 5
  @result = call @factorial(@five)
  ret @result
}
```

### 5.3 Process Communication

```
module @processes

function @counter(@n: int) -> int {
  @start:
  receive {
    atom inc: @increment,
    atom get: @return_value,
    atom stop: @done
  }

  @increment:
  @one = const 1
  @new_n = add @n @one
  tail_call @counter(@new_n)

  @return_value:
  ret @n

  @done:
  @zero = const 0
  ret @zero
}

export function @main() -> int {
  @start:
  @zero = const 0
  @pid = spawn @counter(@zero)
  
  @inc = const atom inc
  send @pid @inc
  send @pid @inc
  
  @get = const atom get
  send @pid @get
  
  @stop = const atom stop
  send @pid @stop
  
  ret @zero
}
```

### 5.4 Pattern Matching

```
module @patterns

function @classify(@x: int) -> atom {
  @start:
  @zero = const 0
  @is_zero = cmp eq @x @zero
  jump_if @is_zero @case_zero @check_positive

  @case_zero:
  @result_zero = const atom zero
  ret @result_zero

  @check_positive:
  @is_positive = cmp gt @x @zero
  jump_if @is_positive @case_positive @case_negative

  @case_positive:
  @result_pos = const atom positive
  ret @result_pos

  @case_negative:
  @result_neg = const atom negative
  ret @result_neg
}
```

### 5.5 Lists and Higher-Order Operations

```
module @lists

function @sum_list(@items: list) -> int {
  @start:
  @len = external erlang length(@items)
  @zero = const 0
  @is_empty = cmp eq @len @zero
  jump_if @is_empty @base_case @recursive_case

  @base_case:
  ret @zero

  @recursive_case:
  @head = external erlang hd(@items)
  @tail = external erlang tl(@items)
  @tail_sum = call @sum_list(@tail)
  @result = add @head @tail_sum
  ret @result
}

export function @main() -> int {
  @start:
  @numbers = list 1 2 3 4 5
  @sum = call @sum_list(@numbers)
  ret @sum
}
```

---

## Syntax Summary

### BNF Notation

```
module      ::= "module" IDENT
function    ::= ["export"] "function" IDENT "(" params ")" "->" TYPE "{" body "}"
params      ::= [IDENT ":" TYPE ("," IDENT ":" TYPE)*]
body        ::= (label | instruction)*
label       ::= IDENT ":"
instruction ::= IDENT "=" expression | "ret" value | "jump" IDENT
              | "jump_if" value IDENT IDENT | "call" IDENT "(" args ")"
              | "tail_call" IDENT "(" args ")" | "send" value value
              | "receive" "{" cases "}" | "match" value "{" cases "}"
expression  ::= "const" value | "add" value value | "sub" value value
              | "mul" value value | "div" value value | "cmp" CMP value value
              | "call" IDENT "(" args ")" | "external" IDENT IDENT "(" args ")"
              | "tuple" INT value* | "tuple_get" value INT
              | "list" value* | "spawn" IDENT "(" args ")"
value       ::= IDENT | INT | FLOAT | STRING | "true" | "false" | "atom" IDENT
```

### Conventions

- `IDENT`: Identifiers starting with `@` for variables, labels, and functions
- `INT`: Integer literals (e.g., `42`, `-17`)
- `FLOAT`: Floating-point literals (e.g., `3.14`, `-0.5`)
- `STRING`: Quoted strings (e.g., `"hello"`)
- `TYPE`: Type names (`int`, `float`, `string`, `atom`, `bool`, etc.)
- `CMP`: Comparison operators (`eq`, `lt`, `lte`, `gt`, `gte`)

---

## Tips and Best Practices

1. **Use SSA form**: Each variable should be assigned exactly once
2. **Label placement**: Place labels at the start of basic blocks
3. **Tail calls**: Use `tail_call` for recursive functions to enable optimization
4. **Process isolation**: Each spawned process has its own memory space
5. **Pattern matching**: Use comprehensive patterns with wildcards to handle all cases
6. **External calls**: Verify Erlang module and function names before use
7. **Type consistency**: Maintain consistent types throughout operations

---

## Error Messages

Common compilation errors:

- **"Parse error: Unrecognised instruction"** - Invalid syntax or unknown instruction
- **"Invalid module name"** - Module name must start with `@`
- **"Missing return"** - Function must end with `ret` statement
- **"Unknown value"** - Variable used before definition (violates SSA)

---

## Further Reading

- [Erlang Documentation](https://www.erlang.org/docs) - For external function reference
- [SSA Form](https://en.wikipedia.org/wiki/Static_single-assignment_form) - Understanding SSA
- [BEAM VM](https://www.erlang.org/blog/a-brief-beam-primer/) - Target VM architecture
