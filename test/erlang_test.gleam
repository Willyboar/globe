import globe/codegen
import globe/types

pub fn generates_basic_function_test() {
  let function =
    types.Function(
      name: "main",
      params: [],
      return_type: types.Named("int"),
      body: [
        types.Label("@start"),
        types.Assign("value", types.Const(types.Int(42))),
        types.Return(types.Var("value")),
      ],
      exported: True,
    )

  let module = types.Module(name: "hello", functions: [function], types: [])

  let assert Ok(generated) = codegen.generate(module)
  assert generated.name == "hello"
  let expected =
    "-module(hello).\n\n"
    <> "-export([main/0]).\n\n"
    <> "main() ->\n"
    <> "  V_VALUE = 42,\n"
    <> "  V_VALUE."
  assert generated.source == expected
}
