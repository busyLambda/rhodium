(* The stage graph. Two stages today: file text, then parsed program. Further stages attach as
   nodes over these; docs/incremental.md has the recipe. *)

type file_text = { text : string; read_error : string option }
type parse_result = (Ast.program, Diagnostics.t list) result

type source = (file_text, Node.input_kind) Node.t
type parsing = (parse_result, Node.computed) Node.t
type diagnostics_node = (Diagnostics.t list, Node.computed) Node.t

type entry = {
  source : source;
  parsing : parsing;
  import_diagnostics : (Diagnostics.t list, Node.input_kind) Node.t;
  diagnostics : diagnostics_node;
  mutable loaded : bool;
}

type t = { files : (string, entry) Hashtbl.t }

let create () = { files = Hashtbl.create 16 }

(* Reading the filesystem is the driver's job, and it happens once per file; an editor-supplied
   [set_source] overrides it. A missing file keeps its error so parsing reports it. *)
let read path =
  match Pipeline.read_source path with
  | text -> { text; read_error = None }
  | exception Sys_error message -> { text = ""; read_error = Some message }

let entry_for t path =
  match Hashtbl.find_opt t.files path with
  | Some entry -> entry
  | None ->
      let source = Node.input ~eq:( = ) { text = ""; read_error = None } in
      let parsing =
        Node.create (fun () ->
            match Node.get source with
            | { read_error = Some message; _ } -> Error [ Diagnostics.error message ]
            | { text; read_error = None } -> Pipeline.parse_string ~filename:path text)
      in
      (* Two sources of trouble: the file's own parse, and what the driver knows about its
         imports. Seeded by [discover], so a driver-level diagnostic is still diffable. *)
      let import_diagnostics = Node.input ~eq:( = ) [] in
      let diagnostics =
        Node.create ~eq:( = ) (fun () ->
            let own = match Node.get parsing with Ok _ -> [] | Error ds -> ds in
            own @ Node.get import_diagnostics)
      in
      let entry = { source; parsing; import_diagnostics; diagnostics; loaded = false } in
      Hashtbl.replace t.files path entry;
      entry

let load_entry t path =
  let entry = entry_for t path in
  if not entry.loaded then (
    entry.loaded <- true;
    Node.set entry.source (read path));
  entry

let load t path = ignore (load_entry t path)

let set_source t path text =
  let entry = entry_for t path in
  entry.loaded <- true;
  Node.set entry.source { text; read_error = None }

let parse_node t path = (load_entry t path).parsing

let diagnostics t path = (load_entry t path).diagnostics

(* The walk is the driver, not a stage: it re-reads the import graph, which is cheap because
   every parse it asks for is memoized. It reports what only the driver can know — unresolved
   imports and cycles — and those become each file's import diagnostics. A parse error is
   already in the parse node, so it is not carried in as well. *)
let discover t ~root =
  let graph = Deps.walk ~root ~parse:(fun path -> Node.get (parse_node t path)) in
  let attributed_to path (d : Diagnostics.t) =
    match d.loc with Some loc -> loc.Loc.file = path | None -> false
  in
  Hashtbl.iter
    (fun path entry ->
      let own = match Node.get entry.parsing with Ok _ -> [] | Error ds -> ds in
      Node.set entry.import_diagnostics
        (graph.Deps.diagnostics
        |> List.filter (fun d -> attributed_to path d && not (List.mem d own))))
    t.files;
  graph

let parses t =
  Hashtbl.fold (fun _ entry acc -> acc + Node.executions entry.parsing) t.files 0

let sources t =
  Hashtbl.fold
    (fun path entry acc -> (path, (Node.get entry.source).text) :: acc)
    t.files []
