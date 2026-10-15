type parsed = {
  filename : string;
  source : string;
  result : (Ast.program, Diagnostics.t list) result;
}

let parse_string ~filename source =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf filename;
  Lexer.reset ();
  let last_lexeme = ref "" in
  let token lexbuf =
    let t = Lexer.next lexbuf in
    last_lexeme := Lexing.lexeme lexbuf;
    t
  in
  try Ok (Parser.program token lexbuf) with
  | Diagnostics.Lex_error (loc, message) -> Error [ Diagnostics.error ~loc message ]
  | Parser.Error ->
      let loc = Loc.of_lexing lexbuf.lex_start_p lexbuf.lex_curr_p in
      let message =
        match !last_lexeme with
        | "" -> "unexpected end of input"
        | lexeme -> Printf.sprintf "syntax error near '%s'" lexeme
      in
      Error [ Diagnostics.error ~loc message ]

let tokens ~filename source =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf filename;
  Lexer.reset ();
  let rec go acc =
    match Lexer.next_lexeme lexbuf with
    | Parser.EOF, _, loc -> Ok (List.rev (("EOF", loc) :: acc))
    | _, text, loc -> go ((text, loc) :: acc)
  in
  try go [] with
  | Diagnostics.Lex_error (loc, message) -> Error [ Diagnostics.error ~loc message ]

let read_source path = if path = "-" then In_channel.input_all In_channel.stdin else In_channel.with_open_text path In_channel.input_all

let parse_file path =
  match read_source path with
  | source -> { filename = path; source; result = parse_string ~filename:path source }
  | exception Sys_error message ->
      { filename = path; source = ""; result = Error [ Diagnostics.error message ] }
