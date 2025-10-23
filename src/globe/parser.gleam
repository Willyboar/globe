//// Parsing routines for Globe IL source files.

import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import globe/types

/// Parser failures.
pub type ParserError {
  MissingModule
  InvalidModule(line: Int, message: String)
  InvalidFunction(line: Int, message: String)
  UnexpectedEndOfFile
  InvalidInstruction(line: Int, message: String)
}

/// Transforms a source string into the Globe module representation.
pub fn parse(source: String) -> Result(types.Module, ParserError) {
  let lines = numbered_lines(source)
  parse_module(lines)
}

type Line =
  #(Int, String)

fn parse_module(lines: List(Line)) -> Result(types.Module, ParserError) {
  case skip_whitespace(lines) {
    [] -> Error(MissingModule)
    [line, ..rest] ->
      case parse_module_header(line) {
        Ok(name) -> parse_items(name, rest, [])
        Error(message) -> {
          let #(line_no, _) = line
          Error(InvalidModule(line_no, message))
        }
      }
  }
}

fn parse_module_header(line: Line) -> Result(String, String) {
  let #(line_no, text) = line
  let trimmed = string.trim(text)
  let words = normalise_words(trimmed)

  case words {
    ["module", name] ->
      case string.starts_with(name, "@") {
        True -> Ok(sanitise_identifier(name))
        False -> Error("Module name must begin with @")
      }

    _ ->
      Error(
        "Expected `module @name` declaration at line " <> int.to_string(line_no),
      )
  }
}

fn parse_items(
  module_name: String,
  lines: List(Line),
  acc: List(types.Function),
) -> Result(types.Module, ParserError) {
  case skip_whitespace(lines) {
    [] -> validate_module_result(module_name, acc)

    [line, ..rest] -> {
      let trimmed = string.trim(element(line))

      // Check for export directive
      case string.starts_with(trimmed, "export function ") {
        True ->
          case parse_function(line, rest, True) {
            Ok(#(function, remaining)) ->
              parse_items(module_name, remaining, [function, ..acc])

            Error(error) -> Error(error)
          }

        False ->
          case string.starts_with(trimmed, "function ") {
            True ->
              case parse_function(line, rest, False) {
                Ok(#(function, remaining)) ->
                  parse_items(module_name, remaining, [function, ..acc])

                Error(error) -> Error(error)
              }

            False -> {
              let #(line_no, _) = line
              Error(InvalidModule(
                line_no,
                "Unexpected text outside of function: " <> trimmed,
              ))
            }
          }
      }
    }
  }
}

fn validate_module_result(
  module_name: String,
  functions: List(types.Function),
) -> Result(types.Module, ParserError) {
  let module =
    types.Module(
      name: module_name,
      functions: list.reverse(functions),
      types: [],
    )
  types.validate_module(module)
  |> result.map_error(fn(error) {
    case error {
      types.FunctionMissingReturn(function) ->
        InvalidFunction(0, "Missing return in function " <> function)

      types.EmptyModule ->
        InvalidModule(0, "Module does not define any functions")
    }
  })
}

fn parse_function(
  header_line: Line,
  after_header: List(Line),
  exported: Bool,
) -> Result(#(types.Function, List(Line)), ParserError) {
  let #(line_no, text) = header_line
  let trimmed = string.trim(text)

  // Remove "export " prefix if present
  let function_text = case string.starts_with(trimmed, "export ") {
    True -> string.trim(drop_left(trimmed, 7))
    False -> trimmed
  }

  case string.ends_with(function_text, "{") {
    False ->
      Error(InvalidFunction(
        line_no,
        "Expected `{` at end of function signature",
      ))

    True -> {
      let signature = string.trim(drop_right(function_text, 1))
      case string.starts_with(signature, "function ") {
        False -> Error(InvalidFunction(line_no, "Malformed function header"))
        True -> {
          let remainder = drop_left(signature, 9)
          case parse_function_signature(line_no, string.trim(remainder)) {
            Ok(#(name, params, return_type)) ->
              parse_body(name, params, return_type, exported, after_header, [])

            Error(error) -> Error(error)
          }
        }
      }
    }
  }
}

fn parse_function_signature(
  line_no: Int,
  signature: String,
) -> Result(#(String, List(types.Parameter), types.Type), ParserError) {
  case string.split(signature, "(") {
    [name_text, params_and_return] -> {
      case string.split(params_and_return, ")") {
        [params_text, return_text] -> {
          let name = sanitise_identifier(string.trim(name_text))
          case parse_parameters(line_no, params_text) {
            Ok(parameters) -> {
              let return_type = parse_return_type(line_no, return_text)
              Ok(#(name, parameters, return_type))
            }

            Error(error) -> Error(error)
          }
        }

        _ -> Error(InvalidFunction(line_no, "Unclosed parameter list"))
      }
    }

    _ -> Error(InvalidFunction(line_no, "Missing parameter list"))
  }
}

fn parse_parameters(
  line_no: Int,
  text: String,
) -> Result(List(types.Parameter), ParserError) {
  let trimmed = string.trim(text)

  case trimmed {
    "" -> Ok([])
    _ -> {
      let parts = string.split(trimmed, ",")
      parse_parameter_parts(line_no, parts, [])
    }
  }
}

fn parse_parameter_parts(
  line_no: Int,
  parts: List(String),
  acc: List(types.Parameter),
) -> Result(List(types.Parameter), ParserError) {
  case parts {
    [] -> Ok(list.reverse(acc))
    [part, ..rest] -> {
      let trimmed = string.trim(part)
      case string.split(trimmed, ":") {
        [name_part, type_part] -> {
          let parameter =
            types.Parameter(
              name: sanitise_identifier(string.trim(name_part)),
              typ: types.Named(string.trim(type_part)),
            )
          parse_parameter_parts(line_no, rest, [parameter, ..acc])
        }

        _ -> Error(InvalidFunction(line_no, "Invalid parameter: " <> trimmed))
      }
    }
  }
}

fn parse_return_type(_line_no: Int, text: String) -> types.Type {
  let trimmed = string.trim(text)

  case normalise_words(trimmed) {
    [] -> types.Named("unit")
    ["->", typ] -> types.Named(typ)
    ["->", ..rest] -> types.Named(string.join(rest, " "))
    _ -> types.Named("unit")
  }
}

fn parse_body(
  name: String,
  params: List(types.Parameter),
  return_type: types.Type,
  exported: Bool,
  lines: List(Line),
  acc: List(types.Instruction),
) -> Result(#(types.Function, List(Line)), ParserError) {
  case skip_whitespace(lines) {
    [] -> Error(UnexpectedEndOfFile)

    [line, ..rest] -> {
      let #(line_no, text) = line
      let trimmed = string.trim(text)

      case trimmed {
        "}" ->
          Ok(#(
            types.Function(
              name: name,
              params: params,
              return_type: return_type,
              body: list.reverse(acc),
              exported: exported,
            ),
            rest,
          ))

        _ ->
          case string.ends_with(trimmed, ":") {
            True -> {
              let label =
                sanitise_identifier(string.trim(drop_right(trimmed, 1)))
              parse_body(name, params, return_type, exported, rest, [
                types.Label(label),
                ..acc
              ])
            }

            False ->
              case string.starts_with(trimmed, "tail_call ") {
                True ->
                  case parse_tail_call(line_no, drop_left(trimmed, 10)) {
                    Ok(instruction) ->
                      parse_body(name, params, return_type, exported, rest, [
                        instruction,
                        ..acc
                      ])
                    Error(error) -> Error(error)
                  }

                False ->
                  case string.starts_with(trimmed, "call ") {
                    True ->
                      case parse_call(line_no, drop_left(trimmed, 5)) {
                        Ok(call_expr) ->
                          parse_body(name, params, return_type, exported, rest, [
                            types.CallStmt(call_expr),
                            ..acc
                          ])
                        Error(error) -> Error(error)
                      }

                    False ->
                      case string.starts_with(trimmed, "ret ") {
                        True ->
                          case parse_value(line_no, drop_left(trimmed, 4)) {
                            Ok(value) ->
                              parse_body(
                                name,
                                params,
                                return_type,
                                exported,
                                rest,
                                [types.Return(value), ..acc],
                              )

                            Error(error) -> Error(error)
                          }

                        False ->
                          case string.starts_with(trimmed, "receive ") {
                            True ->
                              parse_receive_instruction(
                                line_no,
                                drop_left(trimmed, 8),
                                name,
                                params,
                                return_type,
                                exported,
                                rest,
                                acc,
                              )

                            False ->
                              case string.starts_with(trimmed, "send ") {
                                True -> {
                                  // send @pid @message
                                  let send_text = drop_left(trimmed, 5)
                                  case normalise_words(send_text) {
                                    [pid_text, msg_text] ->
                                      case parse_value(line_no, pid_text) {
                                        Ok(pid) ->
                                          case parse_value(line_no, msg_text) {
                                            Ok(message) ->
                                              parse_body(
                                                name,
                                                params,
                                                return_type,
                                                exported,
                                                rest,
                                                [
                                                  types.Send(pid, message),
                                                  ..acc
                                                ],
                                              )
                                            Error(error) -> Error(error)
                                          }
                                        Error(error) -> Error(error)
                                      }
                                    _ ->
                                      Error(InvalidInstruction(
                                        line_no,
                                        "send expects: send <pid> <message>",
                                      ))
                                  }
                                }

                                False ->
                                  case string.starts_with(trimmed, "match ") {
                                    True ->
                                      parse_match_instruction(
                                        line_no,
                                        drop_left(trimmed, 6),
                                        name,
                                        params,
                                        return_type,
                                        exported,
                                        rest,
                                        acc,
                                      )

                                    False ->
                                      case
                                        string.starts_with(trimmed, "jump_if ")
                                      {
                                        True ->
                                          parse_jump_if(
                                            line_no,
                                            drop_left(trimmed, 8),
                                            name,
                                            params,
                                            return_type,
                                            exported,
                                            rest,
                                            acc,
                                          )

                                        False ->
                                          case
                                            string.starts_with(trimmed, "jump ")
                                          {
                                            True -> {
                                              let label =
                                                sanitise_identifier(
                                                  string.trim(drop_left(
                                                    trimmed,
                                                    5,
                                                  )),
                                                )
                                              parse_body(
                                                name,
                                                params,
                                                return_type,
                                                exported,
                                                rest,
                                                [types.Jump(label), ..acc],
                                              )
                                            }

                                            False ->
                                              case split_assignment(trimmed) {
                                                Ok(#(lhs, rhs)) -> {
                                                  let target =
                                                    sanitise_identifier(
                                                      string.trim(lhs),
                                                    )
                                                  let expression_text =
                                                    string.trim(rhs)
                                                  case
                                                    parse_expression(
                                                      line_no,
                                                      expression_text,
                                                    )
                                                  {
                                                    Ok(expression) ->
                                                      parse_body(
                                                        name,
                                                        params,
                                                        return_type,
                                                        exported,
                                                        rest,
                                                        [
                                                          types.Assign(
                                                            target,
                                                            expression,
                                                          ),
                                                          ..acc
                                                        ],
                                                      )

                                                    Error(error) -> Error(error)
                                                  }
                                                }

                                                Error(Nil) ->
                                                  Error(InvalidInstruction(
                                                    line_no,
                                                    "Unrecognised instruction: "
                                                      <> trimmed,
                                                  ))
                                              }
                                          }
                                      }
                                  }
                              }
                          }
                      }
                  }
              }
          }
      }
    }
  }
}

fn parse_match_instruction(
  line_no: Int,
  text: String,
  name: String,
  params: List(types.Parameter),
  return_type: types.Type,
  exported: Bool,
  rest: List(Line),
  acc: List(types.Instruction),
) -> Result(#(types.Function, List(Line)), ParserError) {
  // Expected format: @value { pattern1: @label1, pattern2: @label2 }
  let trimmed = string.trim(text)
  case split_at_top_level_brace(trimmed) {
    Ok(#(value_text, cases_text)) -> {
      case parse_value(line_no, string.trim(value_text)) {
        Ok(value) -> {
          case string.ends_with(cases_text, "}") {
            True -> {
              let cases_inner = string.trim(drop_right(cases_text, 1))
              case parse_match_cases(line_no, cases_inner, []) {
                Ok(cases) ->
                  parse_body(name, params, return_type, exported, rest, [
                    types.Match(value, list.reverse(cases)),
                    ..acc
                  ])
                Error(error) -> Error(error)
              }
            }
            False -> Error(InvalidInstruction(line_no, "match must end with }"))
          }
        }
        Error(error) -> Error(error)
      }
    }
    Error(Nil) ->
      Error(InvalidInstruction(line_no, "match expects format: value { cases }"))
  }
}

fn parse_receive_instruction(
  line_no: Int,
  text: String,
  name: String,
  params: List(types.Parameter),
  return_type: types.Type,
  exported: Bool,
  rest: List(Line),
  acc: List(types.Instruction),
) -> Result(#(types.Function, List(Line)), ParserError) {
  // Expected format: { pattern1: @label1, pattern2: @label2, after @timeout: @timeout_label }
  let trimmed = string.trim(text)
  case string.starts_with(trimmed, "{") {
    True -> {
      case split_at_top_level_brace(trimmed) {
        Ok(#(_, cases_text)) -> {
          case string.ends_with(cases_text, "}") {
            True -> {
              let cases_inner = string.trim(drop_right(cases_text, 1))

              // Check if there's an 'after' clause
              case string.split(cases_inner, ", after ") {
                [cases_only, timeout_part] -> {
                  // Has timeout
                  case string.split_once(timeout_part, ":") {
                    Ok(#(timeout_value_text, timeout_label_text)) -> {
                      case
                        parse_value(line_no, string.trim(timeout_value_text))
                      {
                        Ok(timeout_value) -> {
                          let timeout_label =
                            sanitise_identifier(string.trim(timeout_label_text))
                          case parse_match_cases(line_no, cases_only, []) {
                            Ok(cases) ->
                              parse_body(
                                name,
                                params,
                                return_type,
                                exported,
                                rest,
                                [
                                  types.Receive(
                                    list.reverse(cases),
                                    option.Some(#(timeout_value, timeout_label)),
                                  ),
                                  ..acc
                                ],
                              )
                            Error(error) -> Error(error)
                          }
                        }
                        Error(error) -> Error(error)
                      }
                    }
                    Error(_) ->
                      Error(InvalidInstruction(
                        line_no,
                        "Invalid after clause in receive",
                      ))
                  }
                }

                [cases_only] -> {
                  // No timeout
                  case parse_match_cases(line_no, cases_only, []) {
                    Ok(cases) ->
                      parse_body(name, params, return_type, exported, rest, [
                        types.Receive(list.reverse(cases), option.None),
                        ..acc
                      ])
                    Error(error) -> Error(error)
                  }
                }

                _ ->
                  Error(InvalidInstruction(
                    line_no,
                    "receive expects format: { pattern1: label1, ... }",
                  ))
              }
            }
            False ->
              Error(InvalidInstruction(line_no, "receive must end with }"))
          }
        }
        Error(Nil) ->
          Error(InvalidInstruction(line_no, "receive must be enclosed in { }"))
      }
    }
    False ->
      Error(InvalidInstruction(line_no, "receive must be enclosed in { }"))
  }
}

fn parse_jump_if(
  line_no: Int,
  text: String,
  name: String,
  params: List(types.Parameter),
  return_type: types.Type,
  exported: Bool,
  rest: List(Line),
  acc: List(types.Instruction),
) -> Result(#(types.Function, List(Line)), ParserError) {
  case normalise_words(text) {
    [cond, if_true, if_false] ->
      case parse_value(line_no, cond) {
        Ok(condition) -> {
          let true_label = sanitise_identifier(if_true)
          let false_label = sanitise_identifier(if_false)
          parse_body(name, params, return_type, exported, rest, [
            types.JumpIf(condition, true_label, false_label),
            ..acc
          ])
        }
        Error(error) -> Error(error)
      }
    _ ->
      Error(InvalidInstruction(
        line_no,
        "Invalid jump_if: expected 'jump_if <cond> <true_label> <false_label>'",
      ))
  }
}

// Helper type for expression parsers
type ExprParser =
  #(String, Int, fn(Int, String) -> Result(types.Expression, ParserError))

fn parse_expression(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  let trimmed = string.trim(text)

  // Define expression parsers with their prefixes
  let parsers = [
    #("const ", 6, fn(ln, rest) {
      use value <- result.try(parse_value(ln, rest))
      Ok(types.Const(value))
    }),
    #("copy ", 5, fn(ln, rest) {
      use value <- result.try(parse_value(ln, rest))
      Ok(types.Copy(value))
    }),
    #("spawn ", 6, fn(ln, rest) { parse_spawn(ln, rest) }),
    #("call ", 5, fn(ln, rest) { parse_call(ln, rest) }),
    #("external ", 9, fn(ln, rest) { parse_external(ln, rest) }),
    #("bif ", 4, fn(ln, rest) { parse_bif(ln, rest) }),
    #("cmp ", 4, fn(ln, rest) { parse_compare(ln, rest) }),
    #("list ", 5, fn(ln, rest) { parse_list(ln, rest) }),
    #("tuple ", 6, fn(ln, rest) { parse_tuple(ln, rest) }),
    #("tuple_get ", 10, fn(ln, rest) { parse_tuple_get(ln, rest) }),
    #("bitstring_build ", 16, fn(ln, rest) { parse_bitstring_build(ln, rest) }),
    #("binary_concat ", 14, fn(ln, rest) { parse_binary_concat(ln, rest) }),
    #("binary_size ", 12, fn(ln, rest) { parse_binary_size(ln, rest) }),
    #("binary_slice ", 13, fn(ln, rest) { parse_binary_slice(ln, rest) }),
    #("binary_set ", 11, fn(ln, rest) { parse_binary_set(ln, rest) }),
    #("bitstring_extract ", 18, fn(ln, rest) {
      parse_bitstring_extract(ln, rest)
    }),
    #("bitstring_update ", 17, fn(ln, rest) { parse_bitstring_update(ln, rest) }),
  ]

  // Try each parser in order
  try_expression_parsers(trimmed, line_no, parsers)
}

fn try_expression_parsers(
  text: String,
  line_no: Int,
  parsers: List(ExprParser),
) -> Result(types.Expression, ParserError) {
  case parsers {
    [] -> parse_binary(line_no, text)
    [#(prefix, len, parser), ..rest] ->
      case string.starts_with(text, prefix) {
        True -> parser(line_no, drop_left(text, len))
        False -> try_expression_parsers(text, line_no, rest)
      }
  }
}

fn parse_binary(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    ["add", left, right] ->
      parse_pair(line_no, left, right)
      |> result.map(fn(pair) {
        let #(l, r) = pair
        types.Binary(types.Add, l, r)
      })

    ["sub", left, right] ->
      parse_pair(line_no, left, right)
      |> result.map(fn(pair) {
        let #(l, r) = pair
        types.Binary(types.Sub, l, r)
      })

    ["mul", left, right] ->
      parse_pair(line_no, left, right)
      |> result.map(fn(pair) {
        let #(l, r) = pair
        types.Binary(types.Mul, l, r)
      })

    ["div", left, right] ->
      parse_pair(line_no, left, right)
      |> result.map(fn(pair) {
        let #(l, r) = pair
        types.Binary(types.Div, l, r)
      })

    _ -> Error(InvalidInstruction(line_no, "Unrecognised expression: " <> text))
  }
}

fn parse_compare(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [op, left, right] ->
      parse_pair(line_no, left, right)
      |> result.map(fn(pair) {
        let #(l, r) = pair
        case op {
          "eq" -> types.Compare(types.Eq, l, r)
          "lt" -> types.Compare(types.Lt, l, r)
          "lte" -> types.Compare(types.Lte, l, r)
          "gt" -> types.Compare(types.Gt, l, r)
          "gte" -> types.Compare(types.Gte, l, r)
          _ -> types.Compare(types.Eq, l, r)
        }
      })

    _ -> Error(InvalidInstruction(line_no, "Invalid compare expression"))
  }
}

fn parse_call(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case string.split(text, "(") {
    [name_text, args_and_tail] -> {
      case string.split(args_and_tail, ")") {
        [args_text, _] -> {
          let name = sanitise_identifier(string.trim(name_text))
          case parse_arguments(line_no, args_text, []) {
            Ok(arguments) -> Ok(types.Call(name, arguments))
            Error(error) -> Error(error)
          }
        }

        _ -> Error(InvalidInstruction(line_no, "Unclosed call arguments"))
      }
    }

    _ -> Error(InvalidInstruction(line_no, "Malformed call expression"))
  }
}

fn parse_spawn(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  // spawn @function(@args)
  case string.split(text, "(") {
    [name_text, args_and_tail] -> {
      case string.split(args_and_tail, ")") {
        [args_text, _] -> {
          let name = sanitise_identifier(string.trim(name_text))
          case parse_arguments(line_no, args_text, []) {
            Ok(arguments) -> Ok(types.Spawn(name, arguments))
            Error(error) -> Error(error)
          }
        }

        _ -> Error(InvalidInstruction(line_no, "Unclosed spawn arguments"))
      }
    }

    _ -> Error(InvalidInstruction(line_no, "Malformed spawn expression"))
  }
}

fn parse_tail_call(
  line_no: Int,
  text: String,
) -> Result(types.Instruction, ParserError) {
  case string.split(text, "(") {
    [name_text, args_and_tail] -> {
      case string.split(args_and_tail, ")") {
        [args_text, _] -> {
          let name = sanitise_identifier(string.trim(name_text))
          case parse_arguments(line_no, args_text, []) {
            Ok(arguments) -> Ok(types.TailCall(name, arguments))
            Error(error) -> Error(error)
          }
        }

        _ -> Error(InvalidInstruction(line_no, "Unclosed tail_call arguments"))
      }
    }

    _ -> Error(InvalidInstruction(line_no, "Malformed tail_call expression"))
  }
}

fn parse_external(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  // Expected format: "module function(args)"
  case string.split(text, "(") {
    [module_and_func, args_and_tail] -> {
      case string.split(args_and_tail, ")") {
        [args_text, _] -> {
          let words = normalise_words(module_and_func)
          case words {
            [module, function] ->
              case parse_arguments(line_no, args_text, []) {
                Ok(arguments) -> Ok(types.External(module, function, arguments))
                Error(error) -> Error(error)
              }
            _ ->
              Error(InvalidInstruction(
                line_no,
                "Expected 'external module function(args)'",
              ))
          }
        }

        _ -> Error(InvalidInstruction(line_no, "Unclosed external arguments"))
      }
    }

    _ -> Error(InvalidInstruction(line_no, "Malformed external expression"))
  }
}

fn parse_bif(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  // Expected format: "function(args)"
  case string.split(text, "(") {
    [function_text, args_and_tail] -> {
      case string.split(args_and_tail, ")") {
        [args_text, _] -> {
          let function = string.trim(function_text)
          case parse_arguments(line_no, args_text, []) {
            Ok(arguments) -> Ok(types.Bif(function, arguments))
            Error(error) -> Error(error)
          }
        }

        _ -> Error(InvalidInstruction(line_no, "Unclosed bif arguments"))
      }
    }

    _ -> Error(InvalidInstruction(line_no, "Malformed bif expression"))
  }
}

fn parse_arguments(
  line_no: Int,
  text: String,
  acc: List(types.Value),
) -> Result(List(types.Value), ParserError) {
  let trimmed = string.trim(text)

  case trimmed {
    "" -> Ok(list.reverse(acc))
    _ -> {
      let parts = string.split(trimmed, ",")
      parse_argument_parts(line_no, parts, acc)
    }
  }
}

fn parse_argument_parts(
  line_no: Int,
  parts: List(String),
  acc: List(types.Value),
) -> Result(List(types.Value), ParserError) {
  case parts {
    [] -> Ok(list.reverse(acc))
    [part, ..rest] ->
      case parse_value(line_no, part) {
        Ok(value) -> parse_argument_parts(line_no, rest, [value, ..acc])
        Error(error) -> Error(error)
      }
  }
}

fn parse_pair(
  line_no: Int,
  left: String,
  right: String,
) -> Result(#(types.Value, types.Value), ParserError) {
  use left_value <- result.try(parse_value(line_no, left))
  use right_value <- result.try(parse_value(line_no, right))
  Ok(#(left_value, right_value))
}

fn parse_value(line_no: Int, text: String) -> Result(types.Value, ParserError) {
  let trimmed = string.trim(text)

  case string.starts_with(trimmed, "@") {
    True -> Ok(types.Var(sanitise_identifier(trimmed)))

    False ->
      case trimmed {
        "true" -> Ok(types.Bool(True))
        "false" -> Ok(types.Bool(False))
        _ ->
          case string_starts_and_ends_with_quote(trimmed) {
            True -> Ok(types.String(unquote(trimmed)))

            False ->
              case string.starts_with(trimmed, "atom ") {
                True -> Ok(types.Atom(string.trim(drop_left(trimmed, 5))))

                False ->
                  case parse_int(trimmed) {
                    Ok(value) -> Ok(types.Int(value))
                    Error(_) ->
                      case parse_float(trimmed) {
                        Ok(value) -> Ok(types.Float(value))
                        Error(_) ->
                          Error(InvalidInstruction(
                            line_no,
                            "Unknown value: " <> trimmed,
                          ))
                      }
                  }
              }
          }
      }
  }
}

fn numbered_lines(source: String) -> List(Line) {
  let raw = string.split(source, "\n")
  enumerate(raw, 1)
}

fn enumerate(lines: List(String), index: Int) -> List(Line) {
  case lines {
    [] -> []
    [line, ..rest] -> [#(index, line), ..enumerate(rest, index + 1)]
  }
}

fn skip_whitespace(lines: List(Line)) -> List(Line) {
  case lines {
    [] -> []
    [line, ..rest] -> {
      let trimmed = string.trim(element(line))
      case trimmed {
        "" -> skip_whitespace(rest)
        _ ->
          case string.starts_with(trimmed, "//") {
            True -> skip_whitespace(rest)
            False -> [line, ..rest]
          }
      }
    }
  }
}

fn element(line: Line) -> String {
  let #(_, text) = line
  text
}

fn normalise_words(text: String) -> List(String) {
  string.split(text, " ")
  |> list.filter(fn(word) { word != "" })
}

fn sanitise_identifier(name: String) -> String {
  string.replace(name, "@", "")
}

fn unquote(text: String) -> String {
  drop_right(drop_left(text, 1), 1)
}

fn string_starts_and_ends_with_quote(text: String) -> Bool {
  string.starts_with(text, "\"") && string.ends_with(text, "\"")
}

fn drop_left(text: String, count: Int) -> String {
  let total = string.length(text)

  case count >= total {
    True -> ""
    False -> string.slice(text, count, total - count)
  }
}

fn drop_right(text: String, count: Int) -> String {
  let total = string.length(text)

  case count >= total {
    True -> ""
    False -> string.slice(text, 0, total - count)
  }
}

fn parse_list(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [] -> Ok(types.ListLiteral([]))
    elements ->
      parse_list_elements(line_no, elements, [])
      |> result.map(fn(values) { types.ListLiteral(list.reverse(values)) })
  }
}

fn parse_list_elements(
  line_no: Int,
  elements: List(String),
  acc: List(types.Value),
) -> Result(List(types.Value), ParserError) {
  case elements {
    [] -> Ok(acc)
    [elem, ..rest] ->
      case parse_value(line_no, elem) {
        Ok(value) -> parse_list_elements(line_no, rest, [value, ..acc])
        Error(error) -> Error(error)
      }
  }
}

fn parse_tuple(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [size_text, ..elements] ->
      case parse_int(size_text) {
        Ok(size) ->
          parse_tuple_elements(line_no, elements, [])
          |> result.map(fn(values) { types.Tuple(size, list.reverse(values)) })
        Error(_) ->
          Error(InvalidInstruction(line_no, "Invalid tuple size: " <> size_text))
      }
    _ -> Error(InvalidInstruction(line_no, "Invalid tuple expression"))
  }
}

fn parse_tuple_elements(
  line_no: Int,
  elements: List(String),
  acc: List(types.Value),
) -> Result(List(types.Value), ParserError) {
  case elements {
    [] -> Ok(acc)
    [elem, ..rest] ->
      case parse_value(line_no, elem) {
        Ok(value) -> parse_tuple_elements(line_no, rest, [value, ..acc])
        Error(error) -> Error(error)
      }
  }
}

fn parse_tuple_get(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [tuple_text, index_text] ->
      case parse_value(line_no, tuple_text) {
        Ok(tuple_val) ->
          case parse_int(index_text) {
            Ok(index) -> Ok(types.TupleGet(tuple_val, index))
            Error(_) ->
              Error(InvalidInstruction(
                line_no,
                "Invalid tuple index: " <> index_text,
              ))
          }
        Error(error) -> Error(error)
      }
    _ -> Error(InvalidInstruction(line_no, "Invalid tuple_get expression"))
  }
}

fn parse_bitstring_build(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  let trimmed = string.trim(text)

  case string.starts_with(trimmed, "[") && string.ends_with(trimmed, "]") {
    True -> {
      let inner = string.slice(trimmed, 1, string.length(trimmed) - 2)
      parse_bitstring_parts(line_no, inner)
    }
    False -> Error(InvalidInstruction(line_no, "bitstring_build expects [...]"))
  }
}

fn parse_binary_concat(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [left, right] ->
      parse_pair(line_no, left, right)
      |> result.map(fn(pair) {
        let #(l, r) = pair
        types.BinaryConcat(l, r)
      })
    _ ->
      Error(InvalidInstruction(line_no, "binary_concat expects two arguments"))
  }
}

fn parse_binary_size(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case parse_value(line_no, text) {
    Ok(binary) -> Ok(types.BinarySize(binary))
    Error(error) -> Error(error)
  }
}

fn parse_binary_slice(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [binary_text, position_text, length_text] ->
      case parse_value(line_no, binary_text) {
        Ok(binary) ->
          case parse_value(line_no, position_text) {
            Ok(position) ->
              case parse_value(line_no, length_text) {
                Ok(length) -> Ok(types.BinarySlice(binary, position, length))
                Error(error) -> Error(error)
              }
            Error(error) -> Error(error)
          }
        Error(error) -> Error(error)
      }
    _ ->
      Error(InvalidInstruction(line_no, "binary_slice expects three arguments"))
  }
}

fn parse_binary_set(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [binary_text, position_text, value_text] ->
      case parse_value(line_no, binary_text) {
        Ok(binary) ->
          case parse_value(line_no, position_text) {
            Ok(position) ->
              case parse_value(line_no, value_text) {
                Ok(value) -> Ok(types.BinarySet(binary, position, value))
                Error(error) -> Error(error)
              }
            Error(error) -> Error(error)
          }
        Error(error) -> Error(error)
      }
    _ ->
      Error(InvalidInstruction(line_no, "binary_set expects three arguments"))
  }
}

fn parse_bitstring_extract(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [bitstring_text, position_text, size_text] ->
      case parse_value(line_no, bitstring_text) {
        Ok(bitstring) ->
          case parse_value(line_no, position_text) {
            Ok(position) ->
              case parse_value(line_no, size_text) {
                Ok(size) ->
                  Ok(types.BitstringExtract(bitstring, position, size))
                Error(error) -> Error(error)
              }
            Error(error) -> Error(error)
          }
        Error(error) -> Error(error)
      }
    _ ->
      Error(InvalidInstruction(
        line_no,
        "bitstring_extract expects three arguments",
      ))
  }
}

fn parse_bitstring_update(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  case normalise_words(text) {
    [bitstring_text, position_text, value_text, size_text] ->
      case parse_value(line_no, bitstring_text) {
        Ok(bitstring) ->
          case parse_value(line_no, position_text) {
            Ok(position) ->
              case parse_value(line_no, value_text) {
                Ok(value) ->
                  case parse_value(line_no, size_text) {
                    Ok(size) ->
                      Ok(types.BitstringUpdate(bitstring, position, value, size))
                    Error(error) -> Error(error)
                  }
                Error(error) -> Error(error)
              }
            Error(error) -> Error(error)
          }
        Error(error) -> Error(error)
      }
    _ ->
      Error(InvalidInstruction(
        line_no,
        "bitstring_update expects four arguments",
      ))
  }
}

fn parse_bitstring_parts(
  line_no: Int,
  text: String,
) -> Result(types.Expression, ParserError) {
  let parts = string.split(text, ",")
  parse_bitstring_part_list(line_no, parts, [])
  |> result.map(fn(parts_list) {
    types.BitstringBuild(list.reverse(parts_list))
  })
}

fn parse_bitstring_part_list(
  line_no: Int,
  parts: List(String),
  acc: List(types.BitstringPart),
) -> Result(List(types.BitstringPart), ParserError) {
  case parts {
    [] -> Ok(acc)
    [part, ..rest] -> {
      let trimmed = string.trim(part)
      case normalise_words(trimmed) {
        [value_text, size_text] ->
          case parse_value(line_no, value_text) {
            Ok(value) ->
              case parse_int(size_text) {
                Ok(size) ->
                  parse_bitstring_part_list(line_no, rest, [
                    types.BitstringPart(value, size),
                    ..acc
                  ])
                Error(_) ->
                  Error(InvalidInstruction(
                    line_no,
                    "Invalid bitstring size: " <> size_text,
                  ))
              }
            Error(error) -> Error(error)
          }
        _ ->
          Error(InvalidInstruction(
            line_no,
            "Invalid bitstring part: " <> trimmed,
          ))
      }
    }
  }
}

fn parse_match_cases(
  line_no: Int,
  text: String,
  acc: List(types.MatchCase),
) -> Result(List(types.MatchCase), ParserError) {
  let trimmed = string.trim(text)
  case trimmed {
    "" -> Ok(acc)
    _ -> {
      // Split by comma but respect nested braces for tuple patterns
      case split_by_comma_respecting_braces(trimmed) {
        Ok(#(first_case, rest)) ->
          case parse_single_match_case(line_no, first_case) {
            Ok(match_case) ->
              parse_match_cases(line_no, rest, [match_case, ..acc])
            Error(error) -> Error(error)
          }
        Error(Nil) ->
          // Last case, no more commas
          case parse_single_match_case(line_no, trimmed) {
            Ok(match_case) -> Ok([match_case, ..acc])
            Error(error) -> Error(error)
          }
      }
    }
  }
}

fn parse_single_match_case(
  line_no: Int,
  text: String,
) -> Result(types.MatchCase, ParserError) {
  // Format: pattern: @label
  // We need to find the last : that's not inside << >> or { }
  case split_pattern_and_label(text) {
    Ok(#(pattern_text, label_text)) -> {
      case parse_pattern(line_no, string.trim(pattern_text)) {
        Ok(pattern) -> {
          let label = sanitise_identifier(string.trim(label_text))
          Ok(types.MatchCase(pattern, label))
        }
        Error(error) -> Error(error)
      }
    }
    Error(Nil) ->
      Error(InvalidInstruction(
        line_no,
        "match case must have format: pattern: label",
      ))
  }
}

fn split_pattern_and_label(text: String) -> Result(#(String, String), Nil) {
  // Find the last : that's at depth 0 (not inside << >> or { })
  find_label_separator(text, string.length(text) - 1, 0, 0)
}

fn find_label_separator(
  text: String,
  pos: Int,
  brace_depth: Int,
  angle_depth: Int,
) -> Result(#(String, String), Nil) {
  case pos < 0 {
    True -> Error(Nil)
    False -> {
      let char = string.slice(text, pos, 1)
      case char {
        ":" if brace_depth == 0 && angle_depth == 0 -> {
          let pattern = string.slice(text, 0, pos)
          let label = string.slice(text, pos + 1, string.length(text) - pos - 1)
          Ok(#(pattern, label))
        }
        "}" -> find_label_separator(text, pos - 1, brace_depth + 1, angle_depth)
        "{" -> find_label_separator(text, pos - 1, brace_depth - 1, angle_depth)
        ">" -> {
          // Check for >>
          case pos > 0 && string.slice(text, pos - 1, 1) == ">" {
            True ->
              find_label_separator(text, pos - 2, brace_depth, angle_depth + 1)
            False ->
              find_label_separator(text, pos - 1, brace_depth, angle_depth)
          }
        }
        "<" -> {
          // Check for <<
          case pos > 0 && string.slice(text, pos - 1, 1) == "<" {
            True ->
              find_label_separator(text, pos - 2, brace_depth, angle_depth - 1)
            False ->
              find_label_separator(text, pos - 1, brace_depth, angle_depth)
          }
        }
        _ -> find_label_separator(text, pos - 1, brace_depth, angle_depth)
      }
    }
  }
}

fn parse_pattern(
  line_no: Int,
  text: String,
) -> Result(types.Pattern, ParserError) {
  let trimmed = string.trim(text)

  case trimmed {
    "_" -> Ok(types.WildcardPattern)
    _ ->
      case string.starts_with(trimmed, "@") {
        True -> Ok(types.VariablePattern(sanitise_identifier(trimmed)))
        False ->
          case
            string.starts_with(trimmed, "{") && string.ends_with(trimmed, "}")
          {
            True -> {
              // Tuple pattern: {pattern1, pattern2, ...}
              let inner = string.slice(trimmed, 1, string.length(trimmed) - 2)
              parse_tuple_pattern(line_no, inner, [])
            }
            False ->
              case
                string.starts_with(trimmed, "<<")
                && string.ends_with(trimmed, ">>")
              {
                True -> {
                  // Binary pattern: <<parts>>
                  let inner =
                    string.slice(trimmed, 2, string.length(trimmed) - 4)
                  parse_binary_pattern(line_no, inner, [])
                }
                False ->
                  // Try to parse as literal value
                  case parse_value(line_no, trimmed) {
                    Ok(value) -> Ok(types.LiteralPattern(value))
                    Error(_) ->
                      Error(InvalidInstruction(
                        line_no,
                        "Invalid pattern: " <> trimmed,
                      ))
                  }
              }
          }
      }
  }
}

fn parse_tuple_pattern(
  line_no: Int,
  text: String,
  acc: List(types.Pattern),
) -> Result(types.Pattern, ParserError) {
  let trimmed = string.trim(text)
  case trimmed {
    "" -> Ok(types.TuplePattern(list.reverse(acc)))
    _ -> {
      // Split by comma but respect nested braces
      case split_by_comma_respecting_braces(trimmed) {
        Ok(#(first, rest)) ->
          case parse_pattern(line_no, first) {
            Ok(pattern) -> parse_tuple_pattern(line_no, rest, [pattern, ..acc])
            Error(error) -> Error(error)
          }
        Error(Nil) ->
          case parse_pattern(line_no, trimmed) {
            Ok(pattern) ->
              Ok(types.TuplePattern(list.reverse([pattern, ..acc])))
            Error(error) -> Error(error)
          }
      }
    }
  }
}

fn parse_binary_pattern(
  line_no: Int,
  text: String,
  acc: List(types.BinaryPatternPart),
) -> Result(types.Pattern, ParserError) {
  let trimmed = string.trim(text)
  case trimmed {
    "" -> Ok(types.BinaryPattern(list.reverse(acc)))
    _ -> {
      // Split by comma but respect nested angle brackets
      case split_by_comma_respecting_angles(trimmed) {
        Ok(#(first, rest)) ->
          case parse_binary_pattern_part(line_no, first) {
            Ok(part) -> parse_binary_pattern(line_no, rest, [part, ..acc])
            Error(error) -> Error(error)
          }
        Error(Nil) ->
          case parse_binary_pattern_part(line_no, trimmed) {
            Ok(part) -> Ok(types.BinaryPattern(list.reverse([part, ..acc])))
            Error(error) -> Error(error)
          }
      }
    }
  }
}

fn parse_binary_pattern_part(
  line_no: Int,
  text: String,
) -> Result(types.BinaryPatternPart, ParserError) {
  let trimmed = string.trim(text)

  // Check if it's a literal string like "prefix"
  case string_starts_and_ends_with_quote(trimmed) {
    True -> {
      let str_value = types.String(unquote(trimmed))
      Ok(types.LiteralBinaryPart(str_value))
    }
    False -> {
      // Parse as variable with optional size and type
      // Format: @var:size/type or @var/type or @var
      case string.split(trimmed, ":") {
        [var_text, size_and_type] -> {
          let var_name = sanitise_identifier(string.trim(var_text))
          // Split size/type
          case string.split(size_and_type, "/") {
            [size_text, type_spec] -> {
              // Has both size and type
              case parse_value(line_no, string.trim(size_text)) {
                Ok(size_value) ->
                  Ok(types.SizedBinaryPart(
                    var_name,
                    option.Some(size_value),
                    string.trim(type_spec),
                  ))
                Error(_) ->
                  Error(InvalidInstruction(
                    line_no,
                    "Invalid size in binary pattern: " <> size_text,
                  ))
              }
            }
            [size_text] -> {
              // Just size, no type (defaults to integer)
              case parse_int(string.trim(size_text)) {
                Ok(size_int) ->
                  Ok(types.SizedBinaryPart(
                    var_name,
                    option.Some(types.Int(size_int)),
                    "",
                  ))
                Error(_) ->
                  Error(InvalidInstruction(
                    line_no,
                    "Invalid size in binary pattern: " <> size_text,
                  ))
              }
            }
            _ ->
              Error(InvalidInstruction(
                line_no,
                "Invalid binary pattern part: " <> trimmed,
              ))
          }
        }
        [var_text] -> {
          // Check if it has /type without size
          case string.split(var_text, "/") {
            [var_only, type_spec] -> {
              let var_name = sanitise_identifier(string.trim(var_only))
              Ok(types.SizedBinaryPart(
                var_name,
                option.None,
                string.trim(type_spec),
              ))
            }
            [var_only] -> {
              // Just variable, no size or type (defaults to 8-bit integer)
              let var_name = sanitise_identifier(string.trim(var_only))
              Ok(types.SizedBinaryPart(var_name, option.None, ""))
            }
            _ ->
              Error(InvalidInstruction(
                line_no,
                "Invalid binary pattern part: " <> trimmed,
              ))
          }
        }
        _ ->
          Error(InvalidInstruction(
            line_no,
            "Invalid binary pattern part: " <> trimmed,
          ))
      }
    }
  }
}

fn split_by_comma_respecting_angles(
  text: String,
) -> Result(#(String, String), Nil) {
  split_by_comma_angles_helper(text, 0, 0, "")
}

fn split_by_comma_angles_helper(
  remaining: String,
  index: Int,
  depth: Int,
  acc: String,
) -> Result(#(String, String), Nil) {
  case string.pop_grapheme(remaining) {
    Ok(#("<", rest)) ->
      split_by_comma_angles_helper(rest, index + 1, depth + 1, acc <> "<")
    Ok(#(">", rest)) ->
      split_by_comma_angles_helper(rest, index + 1, depth - 1, acc <> ">")
    Ok(#(",", rest)) if depth == 0 -> Ok(#(acc, rest))
    Ok(#(char, rest)) ->
      split_by_comma_angles_helper(rest, index + 1, depth, acc <> char)
    Error(Nil) -> Error(Nil)
  }
}

// Split by comma but respect brace and angle bracket nesting
fn split_by_comma_respecting_braces(
  text: String,
) -> Result(#(String, String), Nil) {
  split_by_comma_helper(text, 0, 0, 0, "")
}

fn split_by_comma_helper(
  remaining: String,
  index: Int,
  brace_depth: Int,
  angle_depth: Int,
  acc: String,
) -> Result(#(String, String), Nil) {
  case string.pop_grapheme(remaining) {
    Ok(#("{", rest)) ->
      split_by_comma_helper(
        rest,
        index + 1,
        brace_depth + 1,
        angle_depth,
        acc <> "{",
      )
    Ok(#("}", rest)) ->
      split_by_comma_helper(
        rest,
        index + 1,
        brace_depth - 1,
        angle_depth,
        acc <> "}",
      )
    Ok(#("<", rest)) -> {
      // Check if it's << (double angle)
      case string.pop_grapheme(rest) {
        Ok(#("<", rest2)) ->
          split_by_comma_helper(
            rest2,
            index + 2,
            brace_depth,
            angle_depth + 1,
            acc <> "<<",
          )
        _ ->
          split_by_comma_helper(
            rest,
            index + 1,
            brace_depth,
            angle_depth,
            acc <> "<",
          )
      }
    }
    Ok(#(">", rest)) -> {
      // Check if it's >> (double angle)
      case string.pop_grapheme(rest) {
        Ok(#(">", rest2)) ->
          split_by_comma_helper(
            rest2,
            index + 2,
            brace_depth,
            angle_depth - 1,
            acc <> ">>",
          )
        _ ->
          split_by_comma_helper(
            rest,
            index + 1,
            brace_depth,
            angle_depth,
            acc <> ">",
          )
      }
    }
    Ok(#(",", rest)) if brace_depth == 0 && angle_depth == 0 -> Ok(#(acc, rest))
    Ok(#(char, rest)) ->
      split_by_comma_helper(
        rest,
        index + 1,
        brace_depth,
        angle_depth,
        acc <> char,
      )
    Error(Nil) -> Error(Nil)
  }
}

// Find the first top-level opening brace and split there
fn split_at_top_level_brace(text: String) -> Result(#(String, String), Nil) {
  split_at_brace_helper(text, "")
}

fn split_at_brace_helper(
  remaining: String,
  acc: String,
) -> Result(#(String, String), Nil) {
  case string.pop_grapheme(remaining) {
    Ok(#("{", rest)) -> Ok(#(acc, rest))
    Ok(#(char, rest)) -> split_at_brace_helper(rest, acc <> char)
    Error(Nil) -> Error(Nil)
  }
}

fn parse_int(text: String) -> Result(Int, Nil) {
  int.parse(text)
}

fn parse_float(text: String) -> Result(Float, Nil) {
  float.parse(text)
}

// Split an assignment statement on the first = that's not inside a quoted string
fn split_assignment(text: String) -> Result(#(String, String), Nil) {
  split_assignment_helper(text, 0, False)
}

fn split_assignment_helper(
  text: String,
  pos: Int,
  in_quotes: Bool,
) -> Result(#(String, String), Nil) {
  case pos >= string.length(text) {
    True -> Error(Nil)
    False -> {
      let char = string.slice(text, pos, 1)
      case char {
        "\"" -> split_assignment_helper(text, pos + 1, !in_quotes)
        "=" if !in_quotes -> {
          let lhs = string.slice(text, 0, pos)
          let rhs = string.slice(text, pos + 1, string.length(text) - pos - 1)
          Ok(#(lhs, rhs))
        }
        _ -> split_assignment_helper(text, pos + 1, in_quotes)
      }
    }
  }
}
