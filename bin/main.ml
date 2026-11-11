open Cmdliner
open Rhodium

let report (parsed : Pipeline.parsed) =
  match parsed.result with
  | Ok _ -> false
  | Error ds ->
      List.iter (fun d -> prerr_endline (Diagnostics.render ~source:parsed.source d)) ds;
      true

let run_over_files handle paths =
  let failed =
    List.fold_left
      (fun failed path ->
        let parsed = Pipeline.parse_file path in
        if report parsed then true else failed || not (handle parsed))
      false paths
  in
  if failed then exit Cmd.Exit.some_error

let files =
  Arg.(value & pos_all string [ "-" ] & info [] ~docv:"FILE" ~doc:"Files to read, or - for stdin.")

let ast_cmd =
  let doc = "Parse files and dump their AST." in
  let run paths =
    run_over_files
      (fun (parsed : Pipeline.parsed) ->
        match parsed.result with
        | Ok program ->
            print_endline (Pprint.program program);
            true
        | Error _ -> false)
      paths
  in
  Cmd.v (Cmd.info "ast" ~doc) Term.(const run $ files)

let tokens_cmd =
  let doc = "Lex files and dump their token stream." in
  let run paths =
    run_over_files
      (fun (parsed : Pipeline.parsed) ->
        match Pipeline.tokens ~filename:parsed.filename parsed.source with
        | Error ds ->
            List.iter (fun d -> prerr_endline (Diagnostics.to_string d)) ds;
            false
        | Ok ts ->
            List.iter (fun (text, loc) -> Printf.printf "%s\t%s\n" (Loc.to_string loc) text) ts;
            true)
      paths
  in
  Cmd.v (Cmd.info "tokens" ~doc) Term.(const run $ files)

let render_diagnostic sources (d : Diagnostics.t) =
  match d.loc with
  | Some loc -> (
      match List.assoc_opt loc.Loc.file sources with
      | Some source -> Diagnostics.render ~source d
      | None -> Diagnostics.to_string d)
  | None -> Diagnostics.to_string d

let check_cmd =
  let doc = "Parse files and their imports, reporting diagnostics." in
  let run paths =
    let failed =
      List.fold_left
        (fun failed path ->
          let build = Build.create () in
          let graph = Build.discover build ~root:path in
          let sources = Build.sources build in
          let files = List.map (fun (n : Deps.node) -> n.file) graph.Deps.nodes in
          let diagnostics =
            List.concat_map (fun file -> Node.get (Build.diagnostics build file)) files
          in
          List.iter (fun d -> prerr_endline (render_diagnostic sources d)) diagnostics;
          if diagnostics = [] then (
            Printf.printf "%s: ok (%d file(s) parsed, %d parse(s) executed)\n" path
              (List.length files) (Build.parses build);
            failed)
          else true)
        false paths
    in
    if failed then exit Cmd.Exit.some_error
  in
  Cmd.v (Cmd.info "check" ~doc) Term.(const run $ files)

let graph_cmd =
  let doc = "Resolve imports transitively and print the module dependency graph." in
  let run paths =
    let failed =
      List.fold_left
        (fun failed path ->
          let graph = Build.discover (Build.create ()) ~root:path in
          List.iter (fun d -> prerr_endline (Diagnostics.to_string d)) graph.diagnostics;
          print_endline (Deps.render graph);
          failed || graph.diagnostics <> [])
        false paths
    in
    if failed then exit Cmd.Exit.some_error
  in
  Cmd.v (Cmd.info "graph" ~doc) Term.(const run $ files)

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "rhodium" ~version:"0.1.0" ~doc:"The Rhodium compiler.")
          [ ast_cmd; tokens_cmd; check_cmd; graph_cmd ]))
