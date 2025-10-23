import gleeunit/should
import globe/types

pub fn missing_return_fails_validation_test() {
  let function =
    types.Function(
      name: "main",
      params: [],
      return_type: types.Named("int"),
      body: [types.Assign("value", types.Const(types.Int(1)))],
      exported: False,
    )

  let module = types.Module(name: "sample", functions: [function], types: [])

  case types.validate_module(module) {
    Error(types.FunctionMissingReturn(function_name)) ->
      function_name |> should.equal("main")
    _ -> should.fail()
  }
}

pub fn empty_module_fails_validation_test() {
  let module = types.Module(name: "empty", functions: [], types: [])

  case types.validate_module(module) {
    Error(types.EmptyModule) -> Nil
    _ -> should.fail()
  }
}

pub fn valid_module_passes_validation_test() {
  let function =
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
    )

  let module = types.Module(name: "valid", functions: [function], types: [])

  case types.validate_module(module) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }
}

pub fn function_with_return_passes_test() {
  let function =
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
    )

  let module = types.Module(name: "math", functions: [function], types: [])

  case types.validate_module(module) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }
}

pub fn multiple_functions_validation_test() {
  let func1 =
    types.Function(
      name: "helper",
      params: [],
      return_type: types.Named("int"),
      body: [
        types.Label("start"),
        types.Assign("value", types.Const(types.Int(1))),
        types.Return(types.Var("value")),
      ],
      exported: False,
    )

  let func2 =
    types.Function(
      name: "main",
      params: [],
      return_type: types.Named("int"),
      body: [
        types.Label("start"),
        types.Assign("result", types.Call("helper", [])),
        types.Return(types.Var("result")),
      ],
      exported: True,
    )

  let module = types.Module(name: "multi", functions: [func1, func2], types: [])

  case types.validate_module(module) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }
}

pub fn function_with_tail_call_passes_test() {
  let function =
    types.Function(
      name: "loop",
      params: [types.Parameter("n", types.Named("int"))],
      return_type: types.Named("int"),
      body: [
        types.Label("start"),
        types.Assign(
          "check",
          types.Compare(types.Eq, types.Var("n"), types.Int(0)),
        ),
        types.JumpIf(types.Var("check"), "done", "recurse"),
        types.Label("done"),
        types.Return(types.Int(0)),
        types.Label("recurse"),
        types.TailCall("loop", [types.Int(0)]),
      ],
      exported: True,
    )

  let module = types.Module(name: "recursive", functions: [function], types: [])

  case types.validate_module(module) {
    Ok(_) -> Nil
    Error(_) -> should.fail()
  }
}
