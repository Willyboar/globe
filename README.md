![Globe](https://github.com/Willyboar/globe/assets/22755228/54162ccb-8f4e-4e6f-a732-e1e21453b213)

A complete compiler for Globe IL (Intermediate Language) that compiles to Erlang/BEAM. Written in Gleam and distributed as an escript.

## Features

- ✅ **Full Pattern Matching** - literal, variable, wildcard, tuple, and binary patterns
- ✅ **Process Operations** - spawn, send, receive with timeout support
- ✅ **Binary Operations** - concat, size, slice, set
- ✅ **Bitstring Operations** - build, extract, update for protocol work
- ✅ **Control Flow** - jump, jump_if, tail-call optimization
- ✅ **Arithmetic & Comparisons** - add, sub, mul, div, eq, lt, lte, gt, gte
- ✅ **Function Calls & Recursion** - including tail-recursive optimization
- ✅ **External FFI** - call any Erlang function via `external` and `bif`
- ✅ **Module System** - with export directives for BEAM generation
- ✅ **Tuple & List Support** - creation, access, and pattern matching
- ✅ **All Primitive Types** - int, float, string, atom, bool, binary, bitstring
- ✅ **Comprehensive Error Reporting**
- ✅ **34+ Unit Tests** - parser, codegen, and validation

## Documentation

📖 **[Read the complete Globe IL documentation](DOCUMENTATION.md)**

The documentation covers:
- Language syntax and semantics
- All instruction types with examples
- Type system
- Pattern matching
- Concurrency primitives
- Complete working examples
- Best practices and tips

## Installation

### From Source

```sh
git clone https://github.com/Willyboar/globe.git
cd globe
gleam build
gleam run -m gleescript
```

This creates an executable `./globe` escript.

### From Release

Download the pre-built `globe` binary from the [releases page](https://github.com/Willyboar/globe/releases), make it executable, and add it to your PATH:

```sh
chmod +x globe
sudo mv globe /usr/local/bin/
```

## Quick Start

```sh
# Show help
./globe help

# Show version
./globe version

# Compile Globe IL to BEAM
./globe compile examples/factorial.ssa

# Compile and run
./globe compile examples/factorial.ssa --run
```

## Example Programs

### Hello World
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

### Factorial
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

### Process Communication
```
module @processes

export function @main() -> int {
  @start:
  @zero = const 0
  @pid = spawn @worker(@zero)

  @msg = const atom hello
  send @pid @msg

  ret @zero
}

function @worker(@state: int) -> int {
  @start:
  receive {
    atom stop: @done,
    @msg: @continue
  }

  @done:
  ret @state

  @continue:
  tail_call @worker(@state)
}
```

More examples in the `examples/` directory:
- `factorial.ssa` - Recursive factorial
- `fibonacci.ssa` - Fibonacci sequence
- `io_demo.ssa` - I/O operations with formatting
- `process_counter.ssa` - Stateful process example
- `pattern_matching_*.ssa` - Various pattern matching examples
- `bitstring_*.ssa` - Binary protocol examples

## Development

```sh
# Build the project
gleam build

# Run tests (34 tests)
gleam test

# Generate escript
gleam run -m gleescript

# Format code
gleam format
```

## Project Structure

```
globe/
├── src/
│   └── globe/
│       ├── compiler.gleam    # Main compilation pipeline
│       ├── parser.gleam      # IL parser
│       ├── types.gleam       # Type definitions
│       ├── codegen.gleam     # Erlang code generation
│       └── ...
├── test/
│   ├── parser_test.gleam     # Parser tests (19 tests)
│   ├── codegen_test.gleam    # Codegen tests (12 tests)
│   └── types_test.gleam      # Validation tests (6 tests)
├── examples/                  # Example Globe IL programs
├── DOCUMENTATION.md           # Complete language documentation
└── README.md                  # This file
```

## Testing

The project includes comprehensive test coverage:

```sh
gleam test
```

Test suites:
- **Parser tests** (19) - Syntax parsing, error handling, all instruction types
- **Codegen tests** (12) - Erlang code generation verification
- **Types tests** (6) - Module and function validation
- **Integration tests** (2) - End-to-end compilation

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT

## Acknowledgments

- Inspired by [QBE](https://c9x.me/compile/) intermediate language
- Built with [Gleam](https://gleam.run/)
- Targets the [BEAM VM](https://www.erlang.org/)
