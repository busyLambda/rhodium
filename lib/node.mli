(* Pull-based incremental computation. See docs/incremental.md.

   The representation is hidden: a node's memo, its recorded dependencies and its revision
   counters are unreachable outside this module, so the only ways to move a node are [get] and
   [set]. *)

type computed
type input_kind
type ('a, 'k) t

val create : ?eq:('a -> 'a -> bool) -> (unit -> 'a) -> ('a, computed) t
val input : ?eq:('a -> 'a -> bool) -> 'a -> ('a, input_kind) t
val get : ('a, 'k) t -> 'a
val set : ('a, input_kind) t -> 'a -> unit
val executions : ('a, 'k) t -> int
