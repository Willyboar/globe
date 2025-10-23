import gleam/option.{Some}
import gleam/string
import globe/compiler
import globe/fs
import globe/path

const test_root = "build/tests/compiler"

pub fn compile_produces_artifacts_test() {
  let project_dir = path.join(test_root, "artifacts")

  let source_path = path.join(project_dir, "main.ssa")
  let source =
    "module @artifacts\n\n"
    <> "export function @main() -> int {\n"
    <> "@start:\n"
    <> "@value = const 5\n"
    <> "ret @value\n"
    <> "}"
  let assert Ok(Nil) = fs.write(source_path, source)

  let assert Ok(outcome) =
    compiler.compile(source_path, compiler.CompileOptions(run: False))
  assert outcome.erlang_path != ""
  let assert Some(path) = outcome.beam_path
  let assert Ok(_) = fs.read(outcome.erlang_path)
  let assert Ok(True) = fs.file_exists(path)
  assert outcome.executed == False
}

pub fn compile_and_run_main_test() {
  let project_dir = path.join(test_root, "run")

  let source_path = path.join(project_dir, "main.ssa")
  let source =
    "module @runner\n\n"
    <> "export function @main() -> int {\n"
    <> "@start:\n"
    <> "@value = const 21\n"
    <> "@result = add @value @value\n"
    <> "ret @result\n"
    <> "}"
  let assert Ok(Nil) = fs.write(source_path, source)

  let assert Ok(outcome) =
    compiler.compile(source_path, compiler.CompileOptions(run: True))
  assert outcome.executed == True
  let assert Some(output) = outcome.stdout
  assert string.trim(output) == ""
}
