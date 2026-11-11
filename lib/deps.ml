type edge = { spec : Ast.import_decl; resolved : string option }

type node = { file : string; edges : edge list }

type t = { nodes : node list; diagnostics : Diagnostics.t list }

let extension = ".rh"

let resolve ~search path =
  search
  |> List.map (fun dir -> Filename.concat dir (path ^ extension))
  |> List.find_opt Sys.file_exists

(* [walk] resolves the import graph, taking parsing from [parse] so that the caller decides
   what a parse is worth caching. Imports are acyclic: an edge that closes a cycle is reported
   at the import that closes it and is not followed. *)
let walk ~root ~parse =
  let root_dir = Filename.dirname root in
  let cycle start chain =
    let rec from = function
      | [] -> []
      | file :: rest -> if file = start then (file :: rest) @ [ file ] else from rest
    in
    String.concat " -> " (from chain)
  in
  let rec visit stack seen acc file =
    if List.mem file seen then (seen, acc)
    else
      let seen = file :: seen in
      match parse file with
      | Error ds ->
          (* Still a module in the build: its imports are unknown, not absent, so it gets no
             edges. Dropping it here would make its diagnostics unreachable by file. *)
          ( seen,
            {
              nodes = { file; edges = [] } :: acc.nodes;
              diagnostics = acc.diagnostics @ ds;
            } )
      | Ok program ->
          let search = Filename.dirname file :: [ root_dir ] in
          let edges =
            program
            |> List.filter_map (fun (d : Ast.decl) ->
                   match d.desc with Ast.DImport spec -> Some spec | _ -> None)
            |> List.map (fun spec -> { spec; resolved = resolve ~search spec.path })
          in
          let unresolved =
            edges
            |> List.filter_map (fun e ->
                   match e.resolved with
                   | Some _ -> None
                   | None ->
                       Some
                         (Diagnostics.error ~loc:e.spec.span
                            (Printf.sprintf "cannot resolve import '%s'" e.spec.path)))
          in
          let acc =
            {
              nodes = { file; edges } :: acc.nodes;
              diagnostics = acc.diagnostics @ unresolved;
            }
          in
          edges
          |> List.filter_map (fun (e : edge) ->
                 Option.map (fun dep -> (e.spec, dep)) e.resolved)
          |> List.fold_left
               (fun (seen, acc) ((spec : Ast.import_decl), dep) ->
                 if dep = file || List.mem dep stack then
                   let message =
                     Printf.sprintf "import cycle: %s" (cycle dep (List.rev (file :: stack)))
                   in
                   ( seen,
                     {
                       acc with
                       diagnostics =
                         acc.diagnostics @ [ Diagnostics.error ~loc:spec.span message ];
                     } )
                 else visit (file :: stack) seen acc dep)
               (seen, acc)
  in
  let _, acc = visit [] [] { nodes = []; diagnostics = [] } root in
  { nodes = List.rev acc.nodes; diagnostics = acc.diagnostics }

let describe (i : Ast.import_decl) = Pprint.import i

let render t =
  t.nodes
  |> List.map (fun n ->
         let deps =
           List.map
             (fun e ->
               match e.resolved with
               | Some f -> f
               | None -> "?" ^ e.spec.path)
             n.edges
         in
         if deps = [] then n.file else Printf.sprintf "%s -> %s" n.file (String.concat ", " deps))
  |> String.concat "\n"
