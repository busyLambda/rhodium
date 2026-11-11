(* Pull-based incremental computation.

   A node memoizes a value and the revision at which its dependencies were last confirmed.
   [get] re-confirms dependencies (cheap, because they are memoized too) and recomputes only
   when one of them reports a change. [eq] is the cutoff: a recomputation that yields an equal
   value leaves [changed_at] alone, so dependents stop before recomputing.

   Three rules:
   - a value returned by [get] must not be mutated by its consumer;
   - [set] is for inputs, which the kind parameter enforces; everything else is computed;
   - dependencies must be acyclic. Nothing detects a cycle: condense it into a single node
     (docs/incremental.md, "Cycles"). *)

(* [computed] and [input_kind] are phantom: they share one representation but keep the two
   kinds apart in the type checker, so [set] cannot be called on a computed node. *)
type computed
type input_kind

type ('a, 'k) t = {
  id : int;
  compute : unit -> 'a;
  eq : 'a -> 'a -> bool;
  mutable value : 'a option;
  mutable verified_at : int;
  mutable changed_at : int;
  mutable deps : (unit -> int) list;
  mutable executions : int;
}

let revision = ref 0
let collector : ((int * (unit -> int)) -> unit) option ref = ref None

let current () = !revision

let next_id = ref 0

let fresh_id () =
  incr next_id;
  !next_id

let create ?(eq = ( == )) compute : ('a, computed) t =
  {
    id = fresh_id ();
    compute;
    eq;
    value = None;
    verified_at = -1;
    changed_at = -1;
    deps = [];
    executions = 0;
  }

let input ?(eq = ( == )) value : ('a, input_kind) t =
  {
    id = fresh_id ();
    compute = (fun () -> value);
    eq;
    value = Some value;
    verified_at = current ();
    changed_at = current ();
    deps = [];
    executions = 0;
  }

let set (node : ('a, input_kind) t) value =
  match node.value with
  | Some old when node.eq old value -> ()
  | _ ->
      incr revision;
      node.value <- Some value;
      node.changed_at <- current ();
      node.verified_at <- current ()

let rec record : type a k. (a, k) t -> unit =
 fun node ->
  match !collector with
  | Some push -> push (node.id, fun () -> ignore (get node); node.changed_at)
  | None -> ()

and get : type a k. (a, k) t -> a =
 fun node ->
  record node;
  let rev = current () in
  match node.value with
  | Some value when node.verified_at = rev -> value
  | cached ->
      let caller = !collector in
      let unchanged =
        Fun.protect
          ~finally:(fun () -> collector := caller)
          (fun () ->
            collector := None;
            List.for_all (fun changed -> changed () <= node.verified_at) node.deps)
      in
      (match cached with
      | Some value when unchanged ->
          node.verified_at <- rev;
          value
      | _ ->
          let collected = ref [] in
          node.executions <- node.executions + 1;
          let value =
            Fun.protect
              ~finally:(fun () -> collector := caller)
              (fun () ->
                collector :=
                  Some
                    (fun (id, dep) ->
                      (* One dependency per node: a compute may read the same node many times. *)
                      if not (List.exists (fun (seen, _) -> seen = id) !collected) then
                        collected := (id, dep) :: !collected);
                node.compute ())
          in
          node.deps <- List.rev_map snd !collected;
          let same =
            match node.value with Some old -> node.eq old value | None -> false
          in
          node.value <- Some value;
          node.verified_at <- rev;
          if not same then node.changed_at <- rev;
          value)

let executions : type a k. (a, k) t -> int = fun node -> node.executions
