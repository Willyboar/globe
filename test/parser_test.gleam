import gleam/list
import gleeunit/should
import globe/parser
import globe/types

pub fn parses_simple_function_test() {
  let source =
    "module @hello\n\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@value = const 42\n"
    <> "ret @value\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  module.name
  |> should.equal("hello")

  let assert Ok(function) = list.first(module.functions)
  function.name
  |> should.equal("main")

  function.params
  |> should.equal([])

  case function.body {
    [
      types.Label(label),
      types.Assign(name, expression),
      types.Return(return_value),
    ] -> {
      label
      |> should.equal("start")
      name
      |> should.equal("value")

      case expression {
        types.Const(types.Int(value)) -> value |> should.equal(42)
        _ -> should.fail()
      }

      case return_value {
        types.Var(var_name) -> var_name |> should.equal("value")
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn parses_string_literal_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@msg = const \"Hello, World!\"\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.Const(types.String(msg))),
      types.Assign(_, types.Const(types.Int(0))),
      types.Return(_),
    ] -> msg |> should.equal("Hello, World!")
    _ -> should.fail()
  }
}

pub fn parses_string_with_equals_sign_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@format = const \"X = ~p, Y = ~p~n\"\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.Const(types.String(msg))),
      types.Assign(_, types.Const(types.Int(0))),
      types.Return(_),
    ] -> msg |> should.equal("X = ~p, Y = ~p~n")
    _ -> should.fail()
  }
}

pub fn parses_binary_operations_test() {
  let source =
    "module @test\n"
    <> "function @add(@a: int, @b: int) -> int {\n"
    <> "@start:\n"
    <> "@result = add @a @b\n"
    <> "ret @result\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.Binary(types.Add, types.Var("a"), types.Var("b"))),
      types.Return(_),
    ] -> Nil
    _ -> should.fail()
  }
}

pub fn parses_function_call_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@result = call @helper(42, 99)\n"
    <> "ret @result\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.Call("helper", args)),
      types.Return(_),
    ] -> {
      list.length(args) |> should.equal(2)
      case args {
        [types.Int(42), types.Int(99)] -> Nil
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn parses_call_statement_without_assignment_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "call @print(42)\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.CallStmt(types.Call("print", [types.Int(42)])),
      types.Assign(_, _),
      types.Return(_),
    ] -> Nil
    _ -> should.fail()
  }
}

pub fn parses_external_call_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@result = external io format(@msg)\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.External("io", "format", [types.Var("msg")])),
      types.Assign(_, types.Const(types.Int(0))),
      types.Return(_),
    ] -> Nil
    _ -> should.fail()
  }
}

pub fn parses_list_literal_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@items = list 1 2 3\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.ListLiteral(items)),
      types.Assign(_, types.Const(types.Int(0))),
      types.Return(_),
    ] -> {
      list.length(items) |> should.equal(3)
      case items {
        [types.Int(1), types.Int(2), types.Int(3)] -> Nil
        _ -> should.fail()
      }
    }
    _ -> should.fail()
  }
}

pub fn parses_tuple_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@pair = tuple 2 @x @y\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.Tuple(size, elements)),
      types.Assign(_, types.Const(types.Int(0))),
      types.Return(_),
    ] -> {
      size |> should.equal(2)
      list.length(elements) |> should.equal(2)
    }
    _ -> should.fail()
  }
}

pub fn parses_compare_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@check = cmp eq @a @b\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  case function.body {
    [
      types.Label(_),
      types.Assign(_, types.Compare(types.Eq, types.Var("a"), types.Var("b"))),
      types.Assign(_, types.Const(types.Int(0))),
      types.Return(_),
    ] -> Nil
    _ -> should.fail()
  }
}

pub fn parses_exported_function_test() {
  let source =
    "module @test\n"
    <> "export function @main() -> int {\n"
    <> "@start:\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  function.exported |> should.be_true()
}

pub fn parses_non_exported_function_test() {
  let source =
    "module @test\n"
    <> "function @helper() -> int {\n"
    <> "@start:\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  function.exported |> should.be_false()
}

pub fn parses_function_with_parameters_test() {
  let source =
    "module @test\n"
    <> "function @add(@a: int, @b: int) -> int {\n"
    <> "@start:\n"
    <> "@result = add @a @b\n"
    <> "ret @result\n"
    <> "}"

  let assert Ok(module) = parser.parse(source)
  let assert Ok(function) = list.first(module.functions)

  list.length(function.params) |> should.equal(2)

  let assert Ok(param1) = list.first(function.params)
  param1.name |> should.equal("a")

  case function.params {
    [_, param2] -> param2.name |> should.equal("b")
    _ -> should.fail()
  }
}

pub fn rejects_invalid_module_name_test() {
  let source =
    "module hello\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@zero = const 0\n"
    <> "ret @zero\n"
    <> "}"

  case parser.parse(source) {
    Error(parser.InvalidModule(_, _)) -> Nil
    _ -> should.fail()
  }
}

pub fn rejects_missing_return_test() {
  let source =
    "module @test\n"
    <> "function @main() -> int {\n"
    <> "@start:\n"
    <> "@value = const 42\n"
    <> "}"

  case parser.parse(source) {
    Error(_) -> Nil
    // Any error is acceptable for missing return
    Ok(_) -> should.fail()
  }
}
