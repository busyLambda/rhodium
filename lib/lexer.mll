{
open Parser

let error lexbuf message =
  raise
    (Diagnostics.Lex_error
       (Loc.of_lexing lexbuf.Lexing.lex_start_p lexbuf.Lexing.lex_curr_p, message))
}

let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let ident = lower (lower | upper | digit)*
let ctor = upper (lower | upper | digit)*
let ws = [' ' '\t' '\r']

rule token = parse
  | ws+ { token lexbuf }
  | '#' [^ '\n']* { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; NEWLINE }
  | "import" { IMPORT }
  | "type" { TYPE }
  | "fn" { FN }
  | "do" { DO }
  | "end" { END }
  | "match" { MATCH }
  | "->" { ARROW }
  | "..." { ELLIPSIS }
  | '=' { EQ }
  | '|' { BAR }
  | ':' { COLON }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '+' { PLUS }
  | '/' { SLASH }
  | '_' { UNDERSCORE }
  | digit+ as n
      { match int_of_string_opt n with
        | Some v -> INT v
        | None -> error lexbuf (Printf.sprintf "integer literal out of range: %s" n) }
  | ctor as c { CTOR c }
  | ident as x { IDENT x }
  | eof { EOF }
  | _ as c { error lexbuf (Printf.sprintf "unexpected character %C" c) }

{
let queue : (token * string * Loc.t) Queue.t = Queue.create ()
let pending_newline : Loc.t option ref = ref None

let reset () =
  Queue.clear queue;
  pending_newline := None

let lexeme loc text = (text, loc)

let rec next_lexeme lexbuf =
  if not (Queue.is_empty queue) then Queue.pop queue
  else
    match token lexbuf with
    | NEWLINE ->
        pending_newline := Some (Loc.of_lexing lexbuf.lex_start_p lexbuf.lex_curr_p);
        next_lexeme lexbuf
    | BAR ->
        pending_newline := None;
        (BAR, Lexing.lexeme lexbuf, Loc.of_lexing lexbuf.lex_start_p lexbuf.lex_curr_p)
    | EOF -> (
        match !pending_newline with
        | Some loc ->
            pending_newline := None;
            (NEWLINE, "\n", loc)
        | None -> (EOF, "", Loc.of_lexing lexbuf.lex_start_p lexbuf.lex_curr_p))
    | tok ->
        let text = Lexing.lexeme lexbuf in
        let loc = Loc.of_lexing lexbuf.lex_start_p lexbuf.lex_curr_p in
        (match !pending_newline with
        | Some nloc ->
            pending_newline := None;
            Queue.push (tok, text, loc) queue;
            (NEWLINE, "\n", nloc)
        | None -> (tok, text, loc))

let next lexbuf = let tok, _, _ = next_lexeme lexbuf in tok
}
