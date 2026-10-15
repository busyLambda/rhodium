type severity = Error | Warning

type t = { severity : severity; loc : Loc.t option; message : string }

let error ?loc message = { severity = Error; loc; message }
let warning ?loc message = { severity = Warning; loc; message }

exception Lex_error of Loc.t * string

let severity_to_string = function Error -> "error" | Warning -> "warning"

let to_string d =
  match d.loc with
  | Some loc -> Printf.sprintf "%s: %s: %s" (Loc.to_string loc) (severity_to_string d.severity) d.message
  | None -> Printf.sprintf "%s: %s" (severity_to_string d.severity) d.message

let render ~source d =
  let headline = to_string d in
  match d.loc with
  | None -> headline
  | Some loc when loc.start_line <= 0 -> headline
  | Some loc -> (
      match List.nth_opt (String.split_on_char '\n' source) (loc.start_line - 1) with
      | None -> headline
      | Some line ->
          let gutter = String.make (String.length (string_of_int loc.start_line)) ' ' in
          let col = max 1 loc.start_col in
          let width =
            if loc.end_line = loc.start_line then max 1 (loc.end_col - loc.start_col)
            else max 1 (String.length line - col + 2)
          in
          Printf.sprintf "%s\n%s | %s\n%s | %s%s" headline (string_of_int loc.start_line) line
            gutter (String.make (col - 1) ' ') (String.make width '^'))
