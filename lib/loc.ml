type t = {
  file : string;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

let of_lexing (s : Lexing.position) (e : Lexing.position) =
  {
    file = s.pos_fname;
    start_line = s.pos_lnum;
    start_col = s.pos_cnum - s.pos_bol + 1;
    end_line = e.pos_lnum;
    end_col = e.pos_cnum - e.pos_bol + 1;
  }

let synthetic = { file = "<generated>"; start_line = 0; start_col = 0; end_line = 0; end_col = 0 }

let to_string t =
  if t.start_line = t.end_line then
    Printf.sprintf "%s:%d:%d" t.file t.start_line t.start_col
  else Printf.sprintf "%s:%d:%d-%d:%d" t.file t.start_line t.start_col t.end_line t.end_col
