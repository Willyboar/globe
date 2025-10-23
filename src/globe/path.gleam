//// Minimal path utilities for joining and splitting filesystem paths.
import gleam/list
import gleam/string

/// Joins two path segments with a single forward slash.
pub fn join(left: String, right: String) -> String {
  let prefix = trim_trailing_slash(left)
  let suffix = trim_leading_slash(right)

  case prefix {
    "" -> suffix
    _ ->
      case suffix {
        "" -> prefix
        _ -> prefix <> "/" <> suffix
      }
  }
}

/// Returns the parent directory of the provided path.
pub fn dirname(path: String) -> String {
  let trimmed = trim_trailing_slash(path)
  let segments =
    string.split(trimmed, "/")
    |> list.filter(fn(segment) { segment != "" })

  case segments {
    [] -> "."
    [_] -> "."
    _ ->
      segments
      |> list.reverse
      |> list.drop(1)
      |> list.reverse
      |> string.join("/")
  }
}

fn trim_leading_slash(text: String) -> String {
  case string.starts_with(text, "/") {
    True -> trim_leading_slash(drop_left(text, 1))
    False -> text
  }
}

fn trim_trailing_slash(text: String) -> String {
  case string.ends_with(text, "/") {
    True -> trim_trailing_slash(drop_right(text, 1))
    False -> text
  }
}

fn drop_left(text: String, count: Int) -> String {
  let length = string.length(text)

  case count >= length {
    True -> ""
    False -> string.slice(text, count, length - count)
  }
}

fn drop_right(text: String, count: Int) -> String {
  let length = string.length(text)

  case count >= length {
    True -> ""
    False -> string.slice(text, 0, length - count)
  }
}
