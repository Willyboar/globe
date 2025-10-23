//// Erlang code generation for Globe modules.

import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import globe/types

/// Result of translating a Globe module to Erlang source.
pub type GeneratedModule {
  GeneratedModule(name: String, source: String)
}

/// Code generation failures.
pub type CodegenError {
  MissingReturn(String)
}

/// Produces Erlang source for the provided module.
pub fn generate(module: types.Module) -> Result(GeneratedModule, CodegenError) {
  let module_name = erlang_module_name(module.name)
  let exports = export_line(module.functions)

  case gather_sources(module.functions, module_name) {
    Ok(function_sources) -> {
      let header = [
        "-module(" <> module_name <> ").",
        "-export([" <> exports <> "]).",
      ]

      let sections = list.append(header, function_sources)
      let source = string.join(sections, "\n\n")

      Ok(GeneratedModule(name: module_name, source: source))
    }

    Error(error) -> Error(error)
  }
}

fn function_to_source(
  module_name: String,
  function: types.Function,
) -> Result(String, CodegenError) {
  let name = erlang_function_name(function.name)
  let params =
    list.map(function.params, fn(parameter) {
      case parameter {
        types.Parameter(name: param_name, ..) -> erlang_var(param_name)
      }
    })
  let params_text = string.join(params, ", ")
  let header = case params {
    [] -> name <> "() ->"
    _ -> name <> "(" <> params_text <> ") ->"
  }

  body_lines(module_name, function.name, function.body)
  |> result.map(fn(lines) {
    let indented = list.map(lines, fn(line) { "  " <> line })
    let body = string.join(indented, "\n")
    header <> "\n" <> body
  })
}

fn body_lines(
  module_name: String,
  function_name: String,
  instructions: List(types.Instruction),
) -> Result(List(String), CodegenError) {
  // Check if function uses control flow (jumps)
  case has_control_flow(instructions) {
    True ->
      build_lines_with_control_flow(module_name, function_name, instructions)
    False -> build_lines(module_name, function_name, instructions, [])
  }
}

fn has_control_flow(instructions: List(types.Instruction)) -> Bool {
  case
    list.find(instructions, fn(inst) {
      case inst {
        types.Jump(_) -> True
        types.JumpIf(_, _, _) -> True
        types.Match(_, _) -> True
        types.Receive(_, _) -> True
        _ -> False
      }
    })
  {
    Ok(_) -> True
    Error(_) -> False
  }
}

fn build_lines_with_control_flow(
  module_name: String,
  function_name: String,
  instructions: List(types.Instruction),
) -> Result(List(String), CodegenError) {
  // Split instructions into labeled blocks
  // Start with "start" as default label name (will be overwritten by first Label instruction)
  let blocks = split_into_blocks(instructions, "start", [], [])

  // Generate code for blocks with case statements
  generate_blocks_as_case(module_name, function_name, blocks)
}

fn split_into_blocks(
  instructions: List(types.Instruction),
  current_label: String,
  current_block: List(types.Instruction),
  blocks: List(#(String, List(types.Instruction))),
) -> List(#(String, List(types.Instruction))) {
  case instructions {
    [] -> {
      case current_block {
        [] -> list.reverse(blocks)
        _ ->
          list.reverse([#(current_label, list.reverse(current_block)), ..blocks])
      }
    }

    [types.Label(name), ..rest] -> {
      let new_blocks = case current_block {
        [] -> blocks
        _ -> [#(current_label, list.reverse(current_block)), ..blocks]
      }
      split_into_blocks(rest, name, [], new_blocks)
    }

    [inst, ..rest] -> {
      split_into_blocks(rest, current_label, [inst, ..current_block], blocks)
    }
  }
}

fn generate_blocks_as_case(
  module_name: String,
  function_name: String,
  blocks: List(#(String, List(types.Instruction))),
) -> Result(List(String), CodegenError) {
  // For now, generate a simple case-based dispatcher
  case blocks {
    [] -> Error(MissingReturn(function_name))
    [#(_, first_block), ..] ->
      generate_block_code(module_name, function_name, first_block, blocks, [])
  }
}

fn generate_block_code(
  module_name: String,
  function_name: String,
  block: List(types.Instruction),
  all_blocks: List(#(String, List(types.Instruction))),
  acc: List(String),
) -> Result(List(String), CodegenError) {
  case block {
    [] -> Error(MissingReturn(function_name))
    [inst, ..rest] -> {
      case inst {
        types.Assign(name, expr) -> {
          let line =
            erlang_var(name)
            <> " = "
            <> expression_to_erlang(module_name, expr)
            <> ","
          generate_block_code(module_name, function_name, rest, all_blocks, [
            line,
            ..acc
          ])
        }

        types.CallStmt(expr) -> {
          let line = expression_to_erlang(module_name, expr) <> ","
          generate_block_code(module_name, function_name, rest, all_blocks, [
            line,
            ..acc
          ])
        }

        types.Return(value) -> {
          let line = value_to_erlang(module_name, value) <> "."
          Ok(list.reverse([line, ..acc]))
        }

        types.TailCall(name, arguments) -> {
          let call = format_call(module_name, name, arguments)
          let line = call <> "."
          Ok(list.reverse([line, ..acc]))
        }

        types.Jump(label) -> {
          // Find the target block and continue
          case find_block(label, all_blocks) {
            Ok(target_block) ->
              generate_block_code(
                module_name,
                function_name,
                target_block,
                all_blocks,
                acc,
              )
            Error(_) -> Error(MissingReturn(function_name))
          }
        }

        types.JumpIf(cond, true_label, false_label) -> {
          // Generate case statement
          case find_block(true_label, all_blocks) {
            Ok(true_block) -> {
              case find_block(false_label, all_blocks) {
                Ok(false_block) -> {
                  case
                    generate_block_code(
                      module_name,
                      function_name,
                      true_block,
                      all_blocks,
                      [],
                    )
                  {
                    Ok(true_lines) -> {
                      case
                        generate_block_code(
                          module_name,
                          function_name,
                          false_block,
                          all_blocks,
                          [],
                        )
                      {
                        Ok(false_lines) -> {
                          let cond_var = value_to_erlang(module_name, cond)

                          // Remove trailing periods from branches and add semicolons
                          let true_last =
                            remove_trailing_period(list.last(true_lines))
                          let true_init = case list.length(true_lines) {
                            1 -> []
                            _ ->
                              list.take(true_lines, list.length(true_lines) - 1)
                          }
                          let true_branch =
                            list.append(true_init, [true_last <> ";"])

                          let false_last =
                            remove_trailing_period(list.last(false_lines))
                          let false_init = case list.length(false_lines) {
                            1 -> []
                            _ ->
                              list.take(
                                false_lines,
                                list.length(false_lines) - 1,
                              )
                          }
                          let false_branch =
                            list.append(false_init, [false_last])

                          let case_lines =
                            [
                              "case " <> cond_var <> " of",
                              "  true ->",
                            ]
                            |> list.append(
                              list.map(true_branch, fn(l) { "    " <> l }),
                            )
                            |> list.append(["  false ->"])
                            |> list.append(
                              list.map(false_branch, fn(l) { "    " <> l }),
                            )
                            |> list.append(["end."])

                          // JumpIf should be terminal - check if there's more to process
                          case rest {
                            [] ->
                              Ok(
                                list.reverse(list.append(
                                  list.reverse(case_lines),
                                  acc,
                                )),
                              )
                            _ -> {
                              // Continue processing remaining instructions (shouldn't happen in well-formed SSA)
                              generate_block_code(
                                module_name,
                                function_name,
                                rest,
                                all_blocks,
                                list.append(list.reverse(case_lines), acc),
                              )
                            }
                          }
                        }
                        Error(e) -> Error(e)
                      }
                    }
                    Error(e) -> Error(e)
                  }
                }
                Error(_) -> Error(MissingReturn(function_name))
              }
            }
            Error(_) -> Error(MissingReturn(function_name))
          }
        }

        types.Match(value, cases) -> {
          // Generate Erlang case statement with pattern matching
          let value_str = value_to_erlang(module_name, value)

          // Process each match case and generate the corresponding block
          case
            generate_match_branches(
              module_name,
              function_name,
              cases,
              all_blocks,
            )
          {
            Ok(branches) -> {
              let case_header = ["case " <> value_str <> " of"]
              let case_lines = list.append(case_header, branches)
              let final_case_lines = list.append(case_lines, ["end."])

              Ok(list.reverse(list.append(list.reverse(final_case_lines), acc)))
            }
            Error(e) -> Error(e)
          }
        }

        types.Send(pid, message) -> {
          // Generate: Pid ! Message
          let pid_str = value_to_erlang(module_name, pid)
          let msg_str = value_to_erlang(module_name, message)
          let send_line = pid_str <> " ! " <> msg_str
          generate_block_code(module_name, function_name, rest, all_blocks, [
            send_line,
            ..acc
          ])
        }

        types.Receive(cases, timeout) -> {
          // Generate Erlang receive statement
          case
            generate_match_branches(
              module_name,
              function_name,
              cases,
              all_blocks,
            )
          {
            Ok(branches) -> {
              let receive_header = ["receive"]
              let receive_lines = list.append(receive_header, branches)

              // Add timeout clause if present
              let final_lines = case timeout {
                option.Some(#(timeout_value, timeout_label)) -> {
                  let timeout_str = value_to_erlang(module_name, timeout_value)
                  case find_block(timeout_label, all_blocks) {
                    Ok(timeout_block) -> {
                      case
                        generate_block_code(
                          module_name,
                          function_name,
                          timeout_block,
                          all_blocks,
                          [],
                        )
                      {
                        Ok(timeout_lines) -> {
                          let timeout_header =
                            "  after " <> timeout_str <> " ->"
                          let indented_timeout =
                            list.map(timeout_lines, fn(line) { "    " <> line })
                          // Strip trailing period from last line
                          let final_timeout = case
                            list.reverse(indented_timeout)
                          {
                            [] -> indented_timeout
                            [last, ..rest_lines] -> {
                              let trimmed_last = case
                                string.ends_with(last, ".")
                              {
                                True ->
                                  string.slice(last, 0, string.length(last) - 1)
                                False -> last
                              }
                              list.reverse([trimmed_last, ..rest_lines])
                            }
                          }
                          list.append(receive_lines, [
                            timeout_header,
                            ..final_timeout
                          ])
                        }
                        Error(_) -> receive_lines
                      }
                    }
                    Error(_) -> receive_lines
                  }
                }
                option.None -> receive_lines
              }

              let complete_receive = list.append(final_lines, ["end."])
              Ok(list.reverse(list.append(list.reverse(complete_receive), acc)))
            }
            Error(e) -> Error(e)
          }
        }

        types.Label(_) ->
          generate_block_code(module_name, function_name, rest, all_blocks, acc)
      }
    }
  }
}

fn generate_match_branches(
  module_name: String,
  function_name: String,
  cases: List(types.MatchCase),
  all_blocks: List(#(String, List(types.Instruction))),
) -> Result(List(String), CodegenError) {
  generate_match_branches_helper(
    module_name,
    function_name,
    cases,
    all_blocks,
    [],
  )
}

fn generate_match_branches_helper(
  module_name: String,
  function_name: String,
  cases: List(types.MatchCase),
  all_blocks: List(#(String, List(types.Instruction))),
  acc: List(String),
) -> Result(List(String), CodegenError) {
  case cases {
    [] -> Ok(list.reverse(acc))
    [types.MatchCase(pattern, label), ..rest] -> {
      case find_block(label, all_blocks) {
        Ok(block) -> {
          case
            generate_block_code(
              module_name,
              function_name,
              block,
              all_blocks,
              [],
            )
          {
            Ok(block_lines) -> {
              let pattern_str = pattern_to_erlang(module_name, pattern)
              let branch_header = "  " <> pattern_str <> " ->"
              let indented_lines =
                list.map(block_lines, fn(line) { "    " <> line })

              // Strip trailing period from last line and add semicolon if not last case
              let final_lines = case list.reverse(indented_lines) {
                [] -> indented_lines
                [last, ..rest_lines] -> {
                  // Remove trailing period
                  let trimmed_last = case string.ends_with(last, ".") {
                    True -> string.slice(last, 0, string.length(last) - 1)
                    False -> last
                  }
                  // Add semicolon if not the last case
                  let final_last = case rest {
                    [] -> trimmed_last
                    _ -> trimmed_last <> ";"
                  }
                  list.reverse([final_last, ..rest_lines])
                }
              }

              let branch_lines = list.append([branch_header], final_lines)

              generate_match_branches_helper(
                module_name,
                function_name,
                rest,
                all_blocks,
                list.append(list.reverse(branch_lines), acc),
              )
            }
            Error(e) -> Error(e)
          }
        }
        Error(_) -> Error(MissingReturn(function_name))
      }
    }
  }
}

fn pattern_to_erlang(module_name: String, pattern: types.Pattern) -> String {
  case pattern {
    types.WildcardPattern -> "_"
    types.VariablePattern(name) -> erlang_var(name)
    types.LiteralPattern(value) -> value_to_erlang(module_name, value)
    types.TuplePattern(patterns) -> {
      let pattern_strs =
        list.map(patterns, fn(p) { pattern_to_erlang(module_name, p) })
      "{" <> string.join(pattern_strs, ", ") <> "}"
    }
    types.BinaryPattern(parts) -> {
      let part_strs =
        list.map(parts, fn(part) {
          binary_pattern_part_to_erlang(module_name, part)
        })
      "<<" <> string.join(part_strs, ", ") <> ">>"
    }
  }
}

fn binary_pattern_part_to_erlang(
  module_name: String,
  part: types.BinaryPatternPart,
) -> String {
  case part {
    types.LiteralBinaryPart(value) -> {
      // Literal string in binary pattern
      case value {
        types.String(s) -> "\"" <> s <> "\""
        _ -> value_to_erlang(module_name, value)
      }
    }
    types.SizedBinaryPart(variable, size, type_spec) -> {
      let var_str = erlang_var(variable)
      case size, type_spec {
        option.None, "" -> var_str
        option.None, type_str -> var_str <> "/" <> type_str
        option.Some(size_val), "" -> {
          let size_str = value_to_erlang(module_name, size_val)
          var_str <> ":" <> size_str
        }
        option.Some(size_val), type_str -> {
          let size_str = value_to_erlang(module_name, size_val)
          var_str <> ":" <> size_str <> "/" <> type_str
        }
      }
    }
  }
}

fn find_block(
  label: String,
  blocks: List(#(String, List(types.Instruction))),
) -> Result(List(types.Instruction), Nil) {
  case
    list.find(blocks, fn(block) {
      let #(block_label, _) = block
      block_label == label
    })
  {
    Ok(#(_, instructions)) -> Ok(instructions)
    Error(_) -> Error(Nil)
  }
}

fn remove_trailing_period(result: Result(String, Nil)) -> String {
  case result {
    Ok(s) -> {
      case string.ends_with(s, ".") {
        True -> {
          let len = string.length(s)
          string.slice(s, 0, len - 1)
        }
        False -> s
      }
    }
    Error(_) -> ""
  }
}

fn build_lines(
  module_name: String,
  function_name: String,
  instructions: List(types.Instruction),
  acc: List(String),
) -> Result(List(String), CodegenError) {
  case instructions {
    [] -> Error(MissingReturn(function_name))

    [instruction, ..rest] ->
      case instruction {
        types.Label(_) -> build_lines(module_name, function_name, rest, acc)

        types.Assign(name, expression) -> {
          let line =
            erlang_var(name)
            <> " = "
            <> expression_to_erlang(module_name, expression)
            <> ","
          build_lines(module_name, function_name, rest, [line, ..acc])
        }

        types.CallStmt(expression) -> {
          let line = expression_to_erlang(module_name, expression) <> ","
          build_lines(module_name, function_name, rest, [line, ..acc])
        }

        types.Return(value) -> {
          let return_line = value_to_erlang(module_name, value) <> "."
          Ok(list.reverse([return_line, ..acc]))
        }

        types.TailCall(name, arguments) -> {
          let call = format_call(module_name, name, arguments)
          let line = call <> "."
          Ok(list.reverse([line, ..acc]))
        }

        types.Send(pid, message) -> {
          // Send is a side effect, so it gets a comma
          let pid_str = value_to_erlang(module_name, pid)
          let msg_str = value_to_erlang(module_name, message)
          let send_line = pid_str <> " ! " <> msg_str <> ","
          build_lines(module_name, function_name, rest, [send_line, ..acc])
        }

        types.Jump(_)
        | types.JumpIf(_, _, _)
        | types.Match(_, _)
        | types.Receive(_, _) ->
          // These should be handled by build_lines_with_control_flow
          build_lines(module_name, function_name, rest, acc)
      }
  }
}

fn expression_to_erlang(
  module_name: String,
  expression: types.Expression,
) -> String {
  case expression {
    types.Const(value) -> value_to_erlang(module_name, value)
    types.Copy(value) -> value_to_erlang(module_name, value)

    types.Binary(op, left, right) ->
      value_to_erlang(module_name, left)
      <> binary_operator(op)
      <> value_to_erlang(module_name, right)

    types.Compare(op, left, right) ->
      value_to_erlang(module_name, left)
      <> compare_operator(op)
      <> value_to_erlang(module_name, right)

    types.Call(name, arguments) -> format_call(module_name, name, arguments)

    types.Tuple(_size, elements) -> {
      let elem_strs =
        list.map(elements, fn(elem) { value_to_erlang(module_name, elem) })
      "{" <> string.join(elem_strs, ", ") <> "}"
    }

    types.ListLiteral(elements) -> {
      let elem_strs =
        list.map(elements, fn(elem) { value_to_erlang(module_name, elem) })
      "[" <> string.join(elem_strs, ", ") <> "]"
    }

    types.TupleGet(tuple, index) -> {
      let tuple_str = value_to_erlang(module_name, tuple)
      "element(" <> int.to_string(index + 1) <> ", " <> tuple_str <> ")"
    }

    types.BitstringBuild(parts) -> {
      let part_strs =
        list.map(parts, fn(part) {
          case part {
            types.BitstringPart(value, size) -> {
              let val_str = value_to_erlang(module_name, value)
              val_str <> ":" <> int.to_string(size)
            }
          }
        })
      "<<" <> string.join(part_strs, ", ") <> ">>"
    }

    types.External(module, function, arguments) -> {
      let args =
        list.map(arguments, fn(arg) { value_to_erlang(module_name, arg) })
      let args_text = string.join(args, ", ")
      module <> ":" <> function <> "(" <> args_text <> ")"
    }

    types.Bif(function, arguments) -> {
      let args =
        list.map(arguments, fn(arg) { value_to_erlang(module_name, arg) })
      let args_text = string.join(args, ", ")
      function <> "(" <> args_text <> ")"
    }

    types.BinaryConcat(left, right) -> {
      let left_str = value_to_erlang(module_name, left)
      let right_str = value_to_erlang(module_name, right)
      "<<" <> left_str <> "/binary, " <> right_str <> "/binary>>"
    }

    types.BinarySize(binary) -> {
      let bin_str = value_to_erlang(module_name, binary)
      "byte_size(" <> bin_str <> ")"
    }

    types.BinarySlice(binary, position, length) -> {
      let bin_str = value_to_erlang(module_name, binary)
      let pos_str = value_to_erlang(module_name, position)
      let len_str = value_to_erlang(module_name, length)
      "binary:part(" <> bin_str <> ", " <> pos_str <> ", " <> len_str <> ")"
    }

    types.BinarySet(binary, position, value) -> {
      // Create a new binary with byte at position replaced
      // Uses binary:replace to set a single byte
      let bin_str = value_to_erlang(module_name, binary)
      let pos_str = value_to_erlang(module_name, position)
      let val_str = value_to_erlang(module_name, value)
      // Split binary at position, replace byte, concatenate
      "<<(binary:part("
      <> bin_str
      <> ", 0, "
      <> pos_str
      <> "))/binary, "
      <> val_str
      <> ", (binary:part("
      <> bin_str
      <> ", "
      <> pos_str
      <> " + 1, byte_size("
      <> bin_str
      <> ") - "
      <> pos_str
      <> " - 1))/binary>>"
    }

    types.BitstringExtract(bitstring, position, size) -> {
      // Extract bits from a bitstring at given position and size
      let bits_str = value_to_erlang(module_name, bitstring)
      let pos_str = value_to_erlang(module_name, position)
      let size_str = value_to_erlang(module_name, size)
      // Skip 'position' bits, extract 'size' bits
      "<<_:"
      <> pos_str
      <> ", V_EXTRACTED:"
      <> size_str
      <> ", _/bitstring>> = "
      <> bits_str
      <> ", V_EXTRACTED"
    }

    types.BitstringUpdate(bitstring, position, value, size) -> {
      // Update bits in a bitstring at given position with new value
      let bits_str = value_to_erlang(module_name, bitstring)
      let pos_str = value_to_erlang(module_name, position)
      let val_str = value_to_erlang(module_name, value)
      let size_str = value_to_erlang(module_name, size)
      // Split at position, insert new bits, append rest
      "<<V_PREFIX:"
      <> pos_str
      <> "/bitstring, _:"
      <> size_str
      <> ", V_SUFFIX/bitstring>> = "
      <> bits_str
      <> ", <<V_PREFIX/bitstring, "
      <> val_str
      <> ":"
      <> size_str
      <> ", V_SUFFIX/bitstring>>"
    }

    types.Spawn(function, arguments) -> {
      // spawn(fun() -> function(args) end)
      let args =
        list.map(arguments, fn(arg) { value_to_erlang(module_name, arg) })
      let args_text = string.join(args, ", ")
      let func_name = erlang_function_name(function)
      "spawn(fun() -> "
      <> module_name
      <> ":"
      <> func_name
      <> "("
      <> args_text
      <> ") end)"
    }
  }
}

fn value_to_erlang(_module_name: String, value: types.Value) -> String {
  case value {
    types.Var(name) -> erlang_var(name)
    types.Int(value) -> int.to_string(value)
    types.Float(value) -> float.to_string(value)
    types.String(value) -> "<<\"" <> escape_string(value) <> "\">>"
    types.Atom(name) -> name
    types.Bool(True) -> "true"
    types.Bool(False) -> "false"
  }
}

fn binary_operator(op: types.BinaryOp) -> String {
  case op {
    types.Add -> " + "
    types.Sub -> " - "
    types.Mul -> " * "
    types.Div -> " div "
  }
}

fn compare_operator(op: types.CompareOp) -> String {
  case op {
    types.Eq -> " =:= "
    types.Lt -> " < "
    types.Lte -> " =< "
    types.Gt -> " > "
    types.Gte -> " >= "
  }
}

fn export_line(functions: List(types.Function)) -> String {
  functions
  |> list.filter(fn(function) { function.exported })
  |> list.map(fn(function) {
    erlang_function_name(function.name)
    <> "/"
    <> int.to_string(list.length(function.params))
  })
  |> string.join(", ")
}

fn erlang_module_name(name: String) -> String {
  string.lowercase(name)
}

fn erlang_function_name(name: String) -> String {
  string.lowercase(name)
}

fn erlang_var(name: String) -> String {
  "V_" <> string.uppercase(name)
}

fn escape_string(text: String) -> String {
  string.replace(text, "\"", "\\\"")
}

fn format_call(
  module_name: String,
  name: String,
  arguments: List(types.Value),
) -> String {
  let args =
    list.map(arguments, fn(argument) { value_to_erlang(module_name, argument) })
  let args_text = string.join(args, ", ")
  // Use unqualified calls for internal functions (works for both exported and private)
  erlang_function_name(name) <> "(" <> args_text <> ")"
}

fn gather_sources(
  functions: List(types.Function),
  module_name: String,
) -> Result(List(String), CodegenError) {
  collect_sources(functions, [], module_name)
}

fn collect_sources(
  remaining: List(types.Function),
  acc: List(String),
  module_name: String,
) -> Result(List(String), CodegenError) {
  case remaining {
    [] -> Ok(list.reverse(acc))
    [function, ..rest] ->
      case function_to_source(module_name, function) {
        Ok(source) -> collect_sources(rest, [source, ..acc], module_name)
        Error(error) -> Error(error)
      }
  }
}
