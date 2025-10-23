//// High-level compilation pipeline for Globe IL.
import gleam/option.{type Option, None, Some}
import gleam/result
import globe/codegen
import globe/fs
import globe/parser
import globe/types
import globe/path

/// Options governing compilation behaviour.
pub type CompileOptions {
  CompileOptions(run: Bool)
}

/// Description of artifacts produced during compilation.
pub type CompilationOutcome {
  CompilationOutcome(
    module_name: String,
    erlang_path: String,
    beam_path: Option(String),
    executed: Bool,
    stdout: Option(String),
  )
}

/// Failures encountered during compilation.
pub type CompileError {
  SourceRead(fs.FileError)
  Parse(parser.ParserError)
  Validation(types.TypeError)
  Codegen(codegen.CodegenError)
  Emit(fs.FileError)
  ErlangCompile(fs.FileError)
  Execution(fs.FileError)
}

/// Compiles the provided SSA file according to the specification.
pub fn compile(path: String, options: CompileOptions) -> Result(CompilationOutcome, CompileError) {
  fs.read(path)
  |> result.map_error(SourceRead)
  |> result.try(fn(source) {
    parser.parse(source)
    |> result.map_error(Parse)
    |> result.try(fn(parsed) {
      types.validate_module(parsed)
      |> result.map_error(Validation)
      |> result.try(fn(validated) {
        codegen.generate(validated)
        |> result.map_error(Codegen)
        |> result.try(fn(generated) {
          let erlang_dir = erlang_output_dir(path)
          let beam_dir = beam_output_dir(path)
          let erlang_path = path.join(erlang_dir, generated.name <> ".erl")

          fs.write(erlang_path, generated.source)
          |> result.map_error(Emit)
          |> result.try(fn(_) {
            fs.compile_erlang(generated.name, erlang_path, beam_dir)
            |> result.map_error(ErlangCompile)
            |> result.try(fn(beam_path) {
              handle_execution(options, generated.name, beam_dir)
              |> result.map(fn(result) {
                let #(executed, stdout) = result
                CompilationOutcome(
                  module_name: generated.name,
                  erlang_path: erlang_path,
                  beam_path: Some(beam_path),
                  executed: executed,
                  stdout: stdout,
                )
              })
            })
          })
        })
      })
    })
  })
}

fn handle_execution(
  options: CompileOptions,
  module_name: String,
  beam_dir: String,
) -> Result(#(Bool, Option(String)), CompileError) {
  case options {
    CompileOptions(run: True) ->
      fs.run_beam(module_name, beam_dir)
      |> result.map(fn(output) { #(True, Some(output)) })
      |> result.map_error(Execution)

    CompileOptions(run: False) -> Ok(#(False, None))
  }
}

fn erlang_output_dir(path: String) -> String {
  let build_dir = path.join(path.dirname(path), "build")
  path.join(build_dir, "erlang")
}

fn beam_output_dir(path: String) -> String {
  let build_dir = path.join(path.dirname(path), "build")
  path.join(build_dir, "beam")
}
