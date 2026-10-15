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

let check_cmd =
  let doc = "Parse files and report diagnostics without output." in
  let run paths =
    run_over_files
      (fun (parsed : Pipeline.parsed) ->
        match parsed.result with
        | Ok _ ->
            Printf.printf "%s: ok\n" parsed.filename;
            true
        | Error _ -> false)
      paths
  in
  Cmd.v (Cmd.info "check" ~doc) Term.(const run $ files)

let () =
  exit
    (Cmd.eval
       (Cmd.group
          (Cmd.info "rhodium" ~version:"0.1.0" ~doc:"The Rhodium compiler.")
          [ ast_cmd; tokens_cmd; check_cmd ]))
