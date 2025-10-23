//// Core type definitions representing Globe IL constructs and validation helpers.

import gleam/list
import gleam/option.{type Option}

/// A fully parsed Globe module prior to code generation.
pub type Module {
  Module(name: String, functions: List(Function), types: List(TypeDef))
}

/// Function declaration within a module.
pub type Function {
  Function(
    name: String,
    params: List(Parameter),
    return_type: Type,
    body: List(Instruction),
    exported: Bool,
  )
}

/// Function parameter binding.
pub type Parameter {
  Parameter(name: String, typ: Type)
}

/// Type alias or algebraic data type definition.
pub type TypeDef {
  Alias(name: String, value: Type)
  Custom(
    name: String,
    parameters: List(String),
    constructors: List(Constructor),
  )
}

/// Constructor for a custom type.
pub type Constructor {
  Constructor(name: String, fields: List(Type))
}

/// Core type representation used throughout the pipeline.
pub type Type {
  Named(name: String)
  Fn(arguments: List(Type), result: Type)
}

/// SSA instruction lowered to a simplified imperative form.
pub type Instruction {
  Label(name: String)
  Assign(name: String, expression: Expression)
  CallStmt(expression: Expression)
  Return(Value)
  Jump(label: String)
  JumpIf(condition: Value, if_true: String, if_false: String)
  TailCall(name: String, arguments: List(Value))
  Match(value: Value, cases: List(MatchCase))
  Send(pid: Value, message: Value)
  Receive(cases: List(MatchCase), timeout: Option(#(Value, String)))
}

/// Expressions assignable to named SSA variables.
pub type Expression {
  Const(Value)
  Copy(Value)
  Binary(BinaryOp, Value, Value)
  Compare(CompareOp, Value, Value)
  Call(String, List(Value))
  Tuple(size: Int, elements: List(Value))
  TupleGet(tuple: Value, index: Int)
  ListLiteral(elements: List(Value))
  BitstringBuild(parts: List(BitstringPart))
  BitstringExtract(bitstring: Value, position: Value, size: Value)
  BitstringUpdate(bitstring: Value, position: Value, value: Value, size: Value)
  External(module: String, function: String, args: List(Value))
  Bif(function: String, args: List(Value))
  BinaryConcat(left: Value, right: Value)
  BinarySize(binary: Value)
  BinarySlice(binary: Value, position: Value, length: Value)
  BinarySet(binary: Value, position: Value, value: Value)
  Spawn(function: String, args: List(Value))
}

/// Primitive and derived runtime values.
pub type Value {
  Var(String)
  Int(Int)
  Float(Float)
  String(String)
  Atom(String)
  Bool(Bool)
}

/// Binary arithmetic operations.
pub type BinaryOp {
  Add
  Sub
  Mul
  Div
}

/// Comparison operators.
pub type CompareOp {
  Eq
  Lt
  Lte
  Gt
  Gte
}

/// Bitstring part specification.
pub type BitstringPart {
  BitstringPart(value: Value, size: Int)
}

/// Pattern matching case.
pub type MatchCase {
  MatchCase(pattern: Pattern, label: String)
}

/// Pattern for matching.
pub type Pattern {
  LiteralPattern(Value)
  VariablePattern(String)
  TuplePattern(List(Pattern))
  WildcardPattern
  BinaryPattern(List(BinaryPatternPart))
}

/// Binary pattern part.
pub type BinaryPatternPart {
  // Sized binary part: <<@var:size/type>>
  SizedBinaryPart(variable: String, size: Option(Value), type_spec: String)
  // Literal binary part: <<"prefix">>
  LiteralBinaryPart(value: Value)
}

/// Errors surfaced during semantic validation.
pub type TypeError {
  FunctionMissingReturn(function: String)
  EmptyModule
}

/// Returns an empty module with the provided name.
pub fn empty_module(name: String) -> Module {
  Module(name: name, functions: [], types: [])
}

/// Validates the minimal structural invariants required by the compiler.
pub fn validate_module(module: Module) -> Result(Module, TypeError) {
  case module.functions {
    [] -> Error(EmptyModule)
    functions -> validate_functions(module, functions)
  }
}

fn validate_functions(
  module: Module,
  functions: List(Function),
) -> Result(Module, TypeError) {
  case list.find(functions, fn(function) { !has_return(function.body) }) {
    Ok(function) -> {
      let Function(name: name, ..) = function
      Error(FunctionMissingReturn(name))
    }
    Error(_) -> Ok(module)
  }
}

fn has_return(instructions: List(Instruction)) -> Bool {
  case
    list.find(instructions, fn(instruction) {
      case instruction {
        Return(_) -> True
        _ -> False
      }
    })
  {
    Ok(_) -> True
    Error(_) -> False
  }
}
