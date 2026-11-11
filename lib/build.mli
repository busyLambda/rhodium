(* The stage graph. See docs/incremental.md. *)

type parse_result = (Ast.program, Diagnostics.t list) result
type t

val create : unit -> t

(* Seeds one input from the filesystem, once per file; [set_source] overrides it. *)
val load : t -> string -> unit
val set_source : t -> string -> string -> unit

val parse_node : t -> string -> (parse_result, Node.computed) Node.t

(* Everything known to be wrong with one file: its parse, plus what the driver attributed to
   its imports. The node an editor diffs. *)
val diagnostics : t -> string -> (Diagnostics.t list, Node.computed) Node.t

val discover : t -> root:string -> Deps.t
val parses : t -> int
val sources : t -> (string * string) list
