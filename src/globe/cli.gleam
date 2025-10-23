//// Command line interface for the Globe compiler.

import argv
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import globe/codegen
import globe/compiler
import globe/parser
import globe/types

/// Supported CLI commands.
pub type Command {
  Help
  Version
  Compile(input: String, run: Bool)
}

/// Parsing failures when interpreting arguments.
pub type CliError {
  UnknownCommand(command: String)
  MissingArgument(argument: String)
  UnexpectedArguments(command: String, extras: List(String))
}

/// Entry point used by `globe.main`.
pub fn run(version: String) -> Nil {
  let arguments = argv.load().arguments
  let program_name = "globe"

  case parse(arguments) {
    Ok(Help) -> print_help(program_name)
    Ok(Version) -> io.println(version_line(program_name, version))
    Ok(Compile(input, run)) -> execute_compile(input, run, version)

    Error(error) -> handle_error(error, program_name)
  }
}

fn execute_compile(input: String, run: Bool, version: String) -> Nil {
  let options = compiler.CompileOptions(run: run)

  case compiler.compile(input, options) {
    Ok(outcome) -> {
      io.println("Globe " <> version <> " compiled " <> outcome.module_name)
      io.println("  Erlang: " <> outcome.erlang_path)
      case outcome.beam_path {
        Some(path) -> io.println("  BEAM: " <> path)
        None -> Nil
      }
      case outcome.executed {
        True -> io.println("  Execution: completed")
        False -> Nil
      }
      case outcome.stdout {
        Some(output) -> print_execution_output(output)
        None -> Nil
      }
    }

    Error(error) -> print_compile_error(error)
  }
}

fn print_compile_error(error: compiler.CompileError) -> Nil {
  case error {
    compiler.SourceRead(_reason) ->
      io.println_error("Failed to read input file.")

    compiler.Parse(reason) -> {
      io.println_error("Parse error:")
      print_parser_error(reason)
    }

    compiler.Validation(reason) -> {
      io.println_error("Validation error:")
      print_type_error(reason)
    }

    compiler.Codegen(reason) -> {
      io.println_error("Code generation error:")
      print_codegen_error(reason)
    }

    compiler.Emit(_reason) ->
      io.println_error("Failed to write generated Erlang.")

    compiler.ErlangCompile(_reason) ->
      io.println_error("Erlang compilation failed.")

    compiler.Execution(_reason) -> io.println_error("Execution failed.")
  }
}

fn print_parser_error(error: parser.ParserError) -> Nil {
  case error {
    parser.MissingModule -> io.println_error("  Missing module declaration")
    parser.InvalidModule(line, msg) ->
      io.println_error("  Line " <> int.to_string(line) <> ": " <> msg)
    parser.InvalidFunction(line, msg) ->
      io.println_error("  Line " <> int.to_string(line) <> ": " <> msg)
    parser.UnexpectedEndOfFile -> io.println_error("  Unexpected end of file")
    parser.InvalidInstruction(line, msg) ->
      io.println_error("  Line " <> int.to_string(line) <> ": " <> msg)
  }
}

fn print_type_error(error: types.TypeError) -> Nil {
  case error {
    types.FunctionMissingReturn(func) ->
      io.println_error(
        "  Function '" <> func <> "' is missing a return statement",
      )
    types.EmptyModule -> io.println_error("  Module has no functions")
  }
}

fn print_codegen_error(error: codegen.CodegenError) -> Nil {
  case error {
    codegen.MissingReturn(func) ->
      io.println_error("  Function '" <> func <> "' is missing a return")
  }
}

fn print_execution_output(output: String) -> Nil {
  let trimmed = string.trim(output)

  case trimmed {
    "" -> Nil
    _ -> {
      io.println("  Output:")
      trimmed
      |> string.split("\n")
      |> list.filter(fn(line) { line != "" })
      |> list.each(fn(line) { io.println("    " <> line) })
    }
  }
}

fn print_help(program_name: String) -> Nil {
  let usage = [
    "Globe compiler interface",
    "",
    "Usage:",
    "  " <> program_name <> " help                         Show this help",
    "  " <> program_name <> " version                      Show version",
    "  "
      <> program_name
      <> " compile <input.ssa> [--run]   Compile SSA input to BEAM",
  ]
  list.each(usage, io.println)
}

fn handle_error(error: CliError, program_name: String) -> Nil {
  case error {
    UnknownCommand(command) ->
      io.println_error("Unknown command \"" <> command <> "\".")

    MissingArgument(argument) ->
      io.println_error("Missing required argument " <> argument <> ".")

    UnexpectedArguments(command, extras) -> {
      let joined = string.join(extras, ", ")
      io.println_error(
        "Unexpected arguments for command " <> command <> ": " <> joined,
      )
    }
  }

  io.println_error("Run `" <> program_name <> " help` for usage.")
}

fn version_line(program_name: String, version: String) -> String {
  program_name <> " " <> version
}

/// Converts a list of CLI arguments into a command.
pub fn parse(arguments: List(String)) -> Result(Command, CliError) {
  case arguments {
    [] -> Ok(Help)
    ["help"] -> Ok(Help)
    ["--help"] -> Ok(Help)
    ["-h"] -> Ok(Help)
    ["version"] -> Ok(Version)
    ["--version"] -> Ok(Version)
    ["-V"] -> Ok(Version)

    ["compile"] -> Error(MissingArgument("<input.ssa>"))

    ["compile", input] -> Ok(Compile(input, False))

    ["compile", input, flag] ->
      case flag {
        "--run" -> Ok(Compile(input, True))
        other -> Error(UnexpectedArguments("compile", [other]))
      }

    ["compile", _input, "--run", ..extras] ->
      Error(UnexpectedArguments("compile", extras))

    [command, ..extras] ->
      case extras {
        [] -> Error(UnknownCommand(command))
        _ -> Error(UnknownCommand(command))
      }
  }
}
