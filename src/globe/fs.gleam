//// Wrapper utilities for interacting with the file system and external tools.

import gleam/int
import gleam/list
import gleam/string
import globe/path
import simplifile

const exit_marker = "__GLOBE_EXIT__"

/// File system or command execution failure.
pub type FileError {
  ReadError(path: String, reason: String)
  WriteError(path: String, reason: String)
  CreateDirectoryError(path: String, reason: String)
  CommandFailed(command: List(String), exit_code: Int, stderr: String)
}

/// Reads a file into memory.
pub fn read(file_path: String) -> Result(String, FileError) {
  case simplifile.read(file_path) {
    Ok(contents) -> Ok(contents)
    Error(reason) ->
      Error(ReadError(file_path, simplifile_error_to_string(reason)))
  }
}

/// Checks if a file exists.
pub fn file_exists(file_path: String) -> Result(Bool, FileError) {
  case simplifile.is_file(file_path) {
    Ok(exists) -> Ok(exists)
    Error(reason) ->
      Error(ReadError(file_path, simplifile_error_to_string(reason)))
  }
}

/// Writes a file, creating parent directories as required.
pub fn write(file_path: String, contents: String) -> Result(Nil, FileError) {
  let parent = path.dirname(file_path)

  case ensure_directory(parent) {
    Ok(Nil) ->
      case simplifile.write(file_path, contents) {
        Ok(Nil) -> Ok(Nil)
        Error(reason) ->
          Error(WriteError(file_path, simplifile_error_to_string(reason)))
      }

    Error(error) -> Error(error)
  }
}

/// Invokes the Erlang compiler for the generated module.
pub fn compile_erlang(
  module_name: String,
  source_path: String,
  output_dir: String,
) -> Result(String, FileError) {
  case simplifile.create_directory_all(output_dir) {
    Ok(Nil) -> {
      let beam_path = path.join(output_dir, module_name <> ".beam")
      let command = ["erlc", "-o", output_dir, source_path]

      case run_command(command) {
        Ok(_) ->
          case simplifile.is_file(beam_path) {
            Ok(True) -> Ok(beam_path)
            Ok(False) ->
              Error(CommandFailed(
                command: command,
                exit_code: 1,
                stderr: "Beam file was not produced",
              ))
            Error(reason) ->
              Error(CommandFailed(
                command: command,
                exit_code: 1,
                stderr: simplifile_error_to_string(reason),
              ))
          }

        Error(error) -> Error(error)
      }
    }

    Error(reason) ->
      Error(CreateDirectoryError(output_dir, simplifile_error_to_string(reason)))
  }
}

/// Executes the compiled BEAM module if requested.
pub fn run_beam(
  module_name: String,
  beam_dir: String,
) -> Result(String, FileError) {
  let command = [
    "erl",
    "-noshell",
    "-pa",
    beam_dir,
    "-eval",
    module_name <> ":main().",
    "-s",
    "init",
    "stop",
  ]

  run_command(command)
}

fn run_command(command: List(String)) -> Result(String, FileError) {
  let command_text = build_command_string(command)
  let full_command = command_text <> "; echo " <> exit_marker <> "$?"
  let raw_output = os_cmd(full_command)

  case parse_command_output(raw_output) {
    Ok(#(exit_code, output)) ->
      case exit_code {
        0 -> Ok(output)
        code ->
          Error(CommandFailed(command: command, exit_code: code, stderr: output))
      }

    Error(_) ->
      Error(CommandFailed(command: command, exit_code: -1, stderr: raw_output))
  }
}

fn build_command_string(command: List(String)) -> String {
  command
  |> list.map(shell_quote)
  |> string.join(" ")
}

fn parse_command_output(output: String) -> Result(#(Int, String), Nil) {
  let lines = string.split(output, "\n")
  let trimmed_lines = drop_trailing_empty(lines)

  case list.reverse(trimmed_lines) {
    [] -> Error(Nil)

    [last_line, ..rest_reversed] ->
      case string.starts_with(last_line, exit_marker) {
        False -> Error(Nil)
        True -> handle_exit_line(last_line, rest_reversed)
      }
  }
}

fn handle_exit_line(
  line: String,
  rest_reversed: List(String),
) -> Result(#(Int, String), Nil) {
  let exit_text =
    string.replace(line, exit_marker, "")
    |> string.trim

  case parse_int(exit_text) {
    Ok(exit_code) -> {
      let stdout_lines = list.reverse(rest_reversed)
      let stdout = string.join(stdout_lines, "\n")
      Ok(#(exit_code, stdout))
    }

    Error(_) -> Error(Nil)
  }
}

fn drop_trailing_empty(lines: List(String)) -> List(String) {
  lines
  |> list.reverse
  |> drop_leading_empty
  |> list.reverse
}

fn drop_leading_empty(lines: List(String)) -> List(String) {
  case lines {
    [] -> []
    [line, ..rest] ->
      case line {
        "" -> drop_leading_empty(rest)
        _ -> lines
      }
  }
}

fn shell_quote(argument: String) -> String {
  "\"" <> string.replace(argument, "\"", "\\\"") <> "\""
}

fn os_cmd(command: String) -> String {
  os_cmd_raw(binary_to_list(command)) |> list_to_binary
}

fn ensure_directory(pathname: String) -> Result(Nil, FileError) {
  case pathname {
    "." -> Ok(Nil)
    "" -> Ok(Nil)
    _ ->
      case simplifile.create_directory_all(pathname) {
        Ok(Nil) -> Ok(Nil)
        Error(reason) ->
          Error(CreateDirectoryError(
            pathname,
            simplifile_error_to_string(reason),
          ))
      }
  }
}

fn simplifile_error_to_string(error: simplifile.FileError) -> String {
  case error {
    simplifile.Eacces -> "Permission denied"
    simplifile.Eagain -> "Resource temporarily unavailable"
    simplifile.Ebadf -> "Bad file descriptor"
    simplifile.Ebadmsg -> "Bad message"
    simplifile.Ebusy -> "Device or resource busy"
    simplifile.Edeadlk -> "Resource deadlock avoided"
    simplifile.Edeadlock -> "Resource deadlock avoided"
    simplifile.Edquot -> "Disk quota exceeded"
    simplifile.Eexist -> "File exists"
    simplifile.Efault -> "Bad address"
    simplifile.Efbig -> "File too large"
    simplifile.Eftype -> "Inappropriate file type or format"
    simplifile.Eintr -> "Interrupted system call"
    simplifile.Einval -> "Invalid argument"
    simplifile.Eio -> "Input/output error"
    simplifile.Eisdir -> "Is a directory"
    simplifile.Eloop -> "Too many levels of symbolic links"
    simplifile.Emfile -> "Too many open files"
    simplifile.Emlink -> "Too many links"
    simplifile.Emultihop -> "Multihop attempted"
    simplifile.Enametoolong -> "File name too long"
    simplifile.Enfile -> "Too many open files in system"
    simplifile.Enobufs -> "No buffer space available"
    simplifile.Enodev -> "No such device"
    simplifile.Enolck -> "No locks available"
    simplifile.Enolink -> "Link has been severed"
    simplifile.Enoent -> "No such file or directory"
    simplifile.Enomem -> "Not enough memory"
    simplifile.Enospc -> "No space left on device"
    simplifile.Enosr -> "No STREAM resources"
    simplifile.Enostr -> "Not a STREAM"
    simplifile.Enosys -> "Function not implemented"
    simplifile.Enotblk -> "Block device required"
    simplifile.Enotdir -> "Not a directory"
    simplifile.Enotsup -> "Operation not supported"
    simplifile.Enxio -> "No such device or address"
    simplifile.Eopnotsupp -> "Operation not supported"
    simplifile.Eoverflow -> "Value too large"
    simplifile.Eperm -> "Operation not permitted"
    simplifile.Epipe -> "Broken pipe"
    simplifile.Erange -> "Result too large"
    simplifile.Erofs -> "Read-only file system"
    simplifile.Espipe -> "Invalid seek"
    simplifile.Esrch -> "No such process"
    simplifile.Estale -> "Stale file handle"
    simplifile.Etxtbsy -> "Text file busy"
    simplifile.Exdev -> "Invalid cross-device link"
    simplifile.NotUtf8 -> "File contains invalid UTF-8"
    simplifile.Unknown(s) -> "Unknown error: " <> s
  }
}

@external(erlang, "os", "cmd")
fn os_cmd_raw(command: List(Int)) -> List(Int)

@external(erlang, "erlang", "binary_to_list")
fn binary_to_list(binary: String) -> List(Int)

@external(erlang, "erlang", "list_to_binary")
fn list_to_binary(list: List(Int)) -> String

fn parse_int(text: String) -> Result(Int, Nil) {
  int.parse(text)
}
