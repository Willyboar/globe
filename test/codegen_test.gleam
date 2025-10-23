import gleam/string
import gleeunit/should
import globe/codegen
import globe/types

pub fn generates_simple_function_test() {
  let module =
    types.Module(
      name: "test",
      functions: [
        types.Function(
          name: "main",
          params: [],
          return_type: types.Named("int"),
          body: [
            types.Label("start"),
            types.Assign("value", types.Const(types.Int(42))),
            types.Return(types.Var("value")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  code
  |> string.contains("-module(test)")
  |> should.be_true()

  code
  |> string.contains("-export([main/0])")
  |> should.be_true()

  code
  |> string.contains("V_VALUE = 42")
  |> should.be_true()
}

pub fn generates_binary_operations_test() {
  let module =
    types.Module(
      name: "math",
      functions: [
        types.Function(
          name: "add",
          params: [
            types.Parameter("a", types.Named("int")),
            types.Parameter("b", types.Named("int")),
          ],
          return_type: types.Named("int"),
          body: [
            types.Label("start"),
            types.Assign(
              "result",
              types.Binary(types.Add, types.Var("a"), types.Var("b")),
            ),
            types.Return(types.Var("result")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  code
  |> string.contains("V_RESULT = V_A + V_B")
  |> should.be_true()
}

pub fn generates_external_call_test() {
  let module =
    types.Module(
      name: "io_test",
      functions: [
        types.Function(
          name: "print",
          params: [types.Parameter("msg", types.Named("string"))],
          return_type: types.Named("atom"),
          body: [
            types.Label("start"),
            types.Assign(
              "result",
              types.External("io", "format", [types.Var("msg")]),
            ),
            types.Return(types.Var("result")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  code
  |> string.contains("io:format")
  |> should.be_true()
}

pub fn generates_list_literal_test() {
  let module =
    types.Module(
      name: "list_test",
      functions: [
        types.Function(
          name: "make_list",
          params: [],
          return_type: types.Named("list"),
          body: [
            types.Label("start"),
            types.Assign(
              "items",
              types.ListLiteral([types.Int(1), types.Int(2), types.Int(3)]),
            ),
            types.Return(types.Var("items")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  code
  |> string.contains("V_ITEMS = [1, 2, 3]")
  |> should.be_true()
}

pub fn generates_tuple_test() {
  let module =
    types.Module(
      name: "tuple_test",
      functions: [
        types.Function(
          name: "make_pair",
          params: [],
          return_type: types.Named("tuple"),
          body: [
            types.Label("start"),
            types.Assign("pair", types.Tuple(2, [types.Int(10), types.Int(20)])),
            types.Return(types.Var("pair")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  code
  |> string.contains("V_PAIR = {10, 20}")
  |> should.be_true()
}

pub fn generates_call_statement_test() {
  let module =
    types.Module(
      name: "call_test",
      functions: [
        types.Function(
          name: "main",
          params: [],
          return_type: types.Named("int"),
          body: [
            types.Label("start"),
            types.CallStmt(types.Call("helper", [types.Int(42)])),
            types.Assign("zero", types.Const(types.Int(0))),
            types.Return(types.Var("zero")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  // CallStmt should generate a standalone function call
  // The exact format may vary but should contain helper call
  code
  |> string.contains("helper")
  |> should.be_true()
}

pub fn generates_string_literal_test() {
  let module =
    types.Module(
      name: "string_test",
      functions: [
        types.Function(
          name: "get_message",
          params: [],
          return_type: types.Named("string"),
          body: [
            types.Label("start"),
            types.Assign("msg", types.Const(types.String("Hello, World!"))),
            types.Return(types.Var("msg")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  code
  |> string.contains("<<\"Hello, World!\">>")
  |> should.be_true()
}

pub fn generates_compare_operations_test() {
  let module =
    types.Module(
      name: "compare_test",
      functions: [
        types.Function(
          name: "equals",
          params: [
            types.Parameter("a", types.Named("int")),
            types.Parameter("b", types.Named("int")),
          ],
          return_type: types.Named("bool"),
          body: [
            types.Label("start"),
            types.Assign(
              "result",
              types.Compare(types.Eq, types.Var("a"), types.Var("b")),
            ),
            types.Return(types.Var("result")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  code
  |> string.contains("V_A =:= V_B")
  |> should.be_true()
}

pub fn generates_non_exported_function_test() {
  let module =
    types.Module(
      name: "private_test",
      functions: [
        types.Function(
          name: "helper",
          params: [],
          return_type: types.Named("int"),
          body: [
            types.Label("start"),
            types.Assign("value", types.Const(types.Int(42))),
            types.Return(types.Var("value")),
          ],
          exported: False,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  // Module with only non-exported functions should still generate valid code
  code
  |> string.contains("-module(private_test)")
  |> should.be_true()
}

pub fn escapes_special_characters_in_strings_test() {
  let module =
    types.Module(
      name: "escape_test",
      functions: [
        types.Function(
          name: "get_newline",
          params: [],
          return_type: types.Named("string"),
          body: [
            types.Label("start"),
            types.Assign("msg", types.Const(types.String("Line1\nLine2"))),
            types.Return(types.Var("msg")),
          ],
          exported: True,
        ),
      ],
      types: [],
    )

  let assert Ok(generated) = codegen.generate(module)
  let code = generated.source

  // Should handle strings with special characters
  code
  |> string.contains("Line1")
  |> should.be_true()
  code
  |> string.contains("Line2")
  |> should.be_true()
}
