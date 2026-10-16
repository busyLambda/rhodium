type edge = { spec : Ast.import_decl; resolved : string option }

type node = { file : string; edges : edge list }

type t = { nodes : node list; diagnostics : Diagnostics.t list }

let extension = ".rh"

let resolve ~search path =
  search
  |> List.map (fun dir -> Filename.concat dir (path ^ extension))
  |> List.find_opt Sys.file_exists

let build ~root =
  let root_dir = Filename.dirname root in
  let rec visit seen acc file =
    if List.mem file seen then (seen, acc)
    else
      let seen = file :: seen in
      let parsed = Pipeline.parse_file file in
      match parsed.result with
      | Error ds -> (seen, { acc with diagnostics = acc.diagnostics @ ds })
      | Ok program ->
          let search = Filename.dirname file :: [ root_dir ] in
          let edges =
            program
            |> List.filter_map (function Ast.DImport spec -> Some spec | _ -> None)
            |> List.map (fun spec -> { spec; resolved = resolve ~search spec.path })
          in
          let unresolved =
            edges
            |> List.filter_map (fun e ->
                   match e.resolved with
                   | Some _ -> None
                   | None ->
                       Some
                         (Diagnostics.error
                            (Printf.sprintf "cannot resolve import '%s'" e.spec.path)))
          in
          let acc =
            {
              nodes = { file; edges } :: acc.nodes;
              diagnostics = acc.diagnostics @ unresolved;
            }
          in
          let deps = List.filter_map (fun e -> e.resolved) edges in
          List.fold_left (fun (seen, acc) dep -> visit seen acc dep) (seen, acc) deps
  in
  let _, acc = visit [] { nodes = []; diagnostics = [] } root in
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
