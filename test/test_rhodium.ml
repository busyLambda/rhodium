open Rhodium

let parse source =
  match Pipeline.parse_string ~filename:"<test>" source with
  | Ok program -> program
  | Error ds ->
      Alcotest.failf "unexpected diagnostics: %s"
        (String.concat "; " (List.map Diagnostics.to_string ds))

let check name expected source = Alcotest.(check string) name expected (Pprint.program (parse source))

let span_lines (l : Loc.t) = (l.start_line, l.end_line)
let span_cols (l : Loc.t) = (l.start_col, l.end_col)

let test_spans_are_tight () =
  match parse "a = 1 + 2 * 3" with
  | [ { Ast.desc = Ast.DBind (_, rhs); span = dspan } ] -> (
      Alcotest.(check (pair int int)) "decl spans the binding" (1, 14) (span_cols dspan);
      Alcotest.(check (pair int int)) "rhs spans the whole sum" (5, 14) (span_cols rhs.Ast.span);
      match rhs.Ast.desc with
      | Ast.EBinop (Ast.Add, _, { Ast.desc = Ast.EBinop (Ast.Mul, _, _); span; _ }) ->
          Alcotest.(check (pair int int)) "nested factor stays tight" (9, 14) (span_cols span)
      | _ -> Alcotest.fail "expected a nested multiplication")
  | _ -> Alcotest.fail "expected one binding"

let test_block_spans_cover_delimiters () =
  match parse "f = fn() never -> do\n  x = 1\nend" with
  | [ { Ast.desc = Ast.DBind (_, { Ast.desc = Ast.EFn (_, _, body); span = fspan }); _ } ] -> (
      Alcotest.(check (pair int int)) "fn spans to end" (1, 3) (span_lines fspan);
      match body.Ast.desc with
      | Ast.EDo [ s ] ->
          Alcotest.(check (pair int int)) "do spans to end" (1, 3) (span_lines body.Ast.span);
          Alcotest.(check (pair int int)) "stmt stays inside" (2, 2) (span_lines s.Ast.span)
      | _ -> Alcotest.fail "expected a do block with one statement")
  | _ -> Alcotest.fail "expected one binding"

let test_fn () =
  check "function binding" "(let add (fn (a i32, b i32) i32 (+ a b)))"
    "add = fn(a i32, b i32) i32 -> a + b"

let test_tuple_ret () =
  check "tuple return" "(let retTuple (fn (a i32, b i32) {i32, i32} {a, b}))"
    "retTuple = fn(a i32, b i32) {i32, i32} -> {a, b}"

let test_call_with_args () = check "call" "(let a (add 20 20))" "a = add(20, 20)"

let test_apply_to_never () = check "apply to never" "(let a (f))" "a = f()"

let test_bare_fn_value () = check "function value" "(let a f)" "a = f"

let test_main () =
  check "main" "(let main (fn () never (do (= res (add 20 20)) (= {a, b} (retTuple)))))"
    "main = fn() never -> do\n  res = add(20, 20)\n  {a, b} = retTuple()\nend"

let test_ctor_parens () = check "constructor args" "(let a (Pair x y))" "a = Pair(x, y)"

let test_ctor_nullary () = check "nullary constructor" "(let a (Nothing))" "a = Nothing"

let test_match () =
  check "match arms"
    "(let f (fn (xs List[T]) Maybe[T] (match xs ([x:...] (Something x)) (_ (Nothing)))))"
    "f = fn(xs List[T]) Maybe[T] -> match xs\n| [x:...] -> Something(x)\n| _ -> Nothing"

let test_generic_ty () =
  check "generic type" "(let f (fn (xs List[T]) Maybe[T] (Nothing)))" "f = fn(xs List[T]) Maybe[T] -> Nothing"

let test_type_decl () =
  check "type decl"
    "(type List[T] (field capacity u32) (field length u32) (field head opaque_ptr) \
     (global head (fn (list List[T]) Maybe[T] (match list ([x:...] (Something x)) (_ (Nothing))))))"
    "type List[T] = {\n  capacity u32\n  length   u32\n  head     opaque_ptr\n\n  head = \
     fn(list List[T]) Maybe[T] -> match list\n  | [x:...] -> Something(x)\n  | _       -> Nothing\n}"

let test_comments () = check "comments" "(let x 1)" "# leading\nx = 1 # trailing"

let test_import_plain () = check "plain import" "(import mylib/helper)" "import mylib/helper"

let test_import_glob () = check "glob import" "(import mylib/helper/*)" "import mylib/helper/*"

let test_import_group () =
  check "group import" "(import mylib/helper/{f, add})" "import mylib/helper/{f, add}"

let test_import_nested () =
  check "nested group import" "(import mylib/helper/{f, otherModule/{x, p}})"
    "import mylib/helper/{f, otherModule/{x, p}}"

let test_qualified_call () = check "qualified call" "(let a (helper::f))" "a = helper::f()"

let test_division () = check "division precedence" "(let a (+ (/ 1 2) 3))" "a = 1 / 2 + 3"

let test_mul_over_add () = check "mul over add" "(let a (+ 1 (* 2 3)))" "a = 1 + 2 * 3"

let test_parens_override () = check "parens override" "(let a (* (+ 1 2) 3))" "a = (1 + 2) * 3"

let test_left_assoc () = check "left associative" "(let a (- (- 1 2) 3))" "a = 1 - 2 - 3"

let test_unary_neg () = check "unary neg" "(let a (+ (- 1) 2))" "a = -1 + 2"

let test_unary_not () = check "unary not" "(let a (not x))" "a = not x"

let test_not_binds_tighter_than_and () =
  check "not binds tighter than and" "(let a (and (not x) y))" "a = not x and y"

let test_and_over_or () = check "and over or" "(let a (or x (and y z)))" "a = x or y and z"

let test_nor () = check "nor" "(let a (nor x y))" "a = x nor y"

let test_xor () = check "xor" "(let a (xor x y))" "a = x xor y"

let test_comparison_over_and () =
  check "comparison over and" "(let a (and (< 1 2) (> 3 4)))" "a = 1 < 2 and 3 > 4"

let test_equality_precedence () =
  check "equality after arithmetic" "(let a (is (+ 1 2) 3))" "a = 1 + 2 is 3"

let test_isnt () = check "isnt" "(let a (isnt x y))" "a = x isnt y"

let test_isnt_precedence () =
  check "isnt precedence" "(let a (and (isnt x y) z))" "a = x isnt y and z"

let test_is_not_is_compositional () =
  check "is not composes" "(let a (is x (not y)))" "a = x is not y"

let test_isnt_keeps_ident () = check "isnt is a keyword" "(let a (is x notx))" "a = x is notx"

let test_ctor_multi_args () = check "ctor multi args" "(let a (Some x y))" "a = Some(x, y)"

let test_ctor_arg_is_call () = check "ctor arg is call" "(let a (Some (f 1)))" "a = Some(f(1))"

let test_ctor_parens_disambiguate () =
  check "ctor parens disambiguate" "(let a (- (Some x) 1))" "a = Some(x) - 1"

let test_ctor_juxtaposition_rejected () =
  match Pipeline.parse_string ~filename:"<test>" "a = Some x" with
  | Ok _ -> Alcotest.fail "expected juxtaposed constructor args to be rejected"
  | Error _ -> ()

let test_stmt_needs_newline () =
  match Pipeline.parse_string ~filename:"<test>" "a = do x y end" with
  | Ok _ -> Alcotest.fail "expected statements to require a newline separator"
  | Error _ -> ()

let test_decl_needs_newline () =
  match Pipeline.parse_string ~filename:"<test>" "x = 1 y = 2" with
  | Ok _ -> Alcotest.fail "expected declarations to require a newline separator"
  | Error _ -> ()

let test_mod_precedence () = check "mod with mul" "(let a (* (% 2 3) 4))" "a = 2 % 3 * 4"

let test_pipe () = check "pipe" "(let a (|> x g))" "a = x |> g"

let test_nested_parens () = check "nested parens" "(let a 1)" "a = ((1))"

let test_nonassoc_rejected () =
  match Pipeline.parse_string ~filename:"<test>" "a = 1 < 2 < 3" with
  | Ok _ -> Alcotest.fail "expected nonassoc comparison to be rejected"
  | Error ds -> Alcotest.(check bool) "rejected" true (ds <> [])

let test_syntax_error () =
  match Pipeline.parse_string ~filename:"<test>" "x = )" with
  | Ok _ -> Alcotest.fail "expected a syntax error"
  | Error ds -> Alcotest.(check bool) "reports a diagnostic" true (ds <> [])

let test_lex_error () =
  match Pipeline.parse_string ~filename:"<test>" "x = @" with
  | Ok _ -> Alcotest.fail "expected a lex error"
  | Error ds ->
      Alcotest.(check bool) "mentions the character" true
        (List.exists (fun (d : Diagnostics.t) -> String.length d.message > 0) ds)

let test_token_positions () =
  match Pipeline.tokens ~filename:"t.rho" "# comment\nx = 1" with
  | Error _ -> Alcotest.fail "expected tokens"
  | Ok ts -> (
      match List.filter (fun (text, _) -> text <> "\n") ts with
      | (_, loc) :: _ ->
          Alcotest.(check string) "first real token is on line 2" "t.rho:2:1" (Loc.to_string loc)
      | [] -> Alcotest.fail "expected tokens")

let test_newline_suppressed_before_bar () =
  match Pipeline.tokens ~filename:"t.rho" "| a\n| b" with
  | Error _ -> Alcotest.fail "expected tokens"
  | Ok ts ->
      Alcotest.(check (list string)) "no newline before bar" [ "|"; "a"; "|"; "b"; "EOF" ]
        (List.map fst ts)

let test_deps () =
  let dir = Filename.temp_file "rhodium_deps" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let write relative content =
    Out_channel.with_open_text (Filename.concat dir relative) (fun oc ->
        Out_channel.output_string oc content)
  in
  Unix.mkdir (Filename.concat dir "mylib") 0o755;
  write "main.rh" "import mylib/helper\n\nmain = fn() never -> do\n  res = helper::add(1, 2)\nend\n";
  write "mylib/helper.rh" "add = fn(a i32, b i32) i32 -> a + b\n";
  let graph = Build.discover (Build.create ()) ~root:(Filename.concat dir "main.rh") in
  Alcotest.(check int) "main plus one dependency" 2 (List.length graph.nodes);
  Alcotest.(check (list string)) "no diagnostics" []
    (List.map Diagnostics.to_string graph.diagnostics);
  Alcotest.(check string) "graph rendering"
    (Printf.sprintf "%s -> %s\n%s" (Filename.concat dir "main.rh")
       (Filename.concat dir "mylib/helper.rh")
       (Filename.concat dir "mylib/helper.rh"))
    (Deps.render graph)

let test_deps_cycle () =
  let dir = Filename.temp_file "rhodium_cycle" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let write name content =
    Out_channel.with_open_text (Filename.concat dir name) (fun oc ->
        Out_channel.output_string oc content)
  in
  write "a.rh" "import b\n\na = 1\n";
  write "b.rh" "import a\n\nb = 2\n";
  let build = Build.create () in
  let root = Filename.concat dir "a.rh" in
  let graph = Build.discover build ~root in
  Alcotest.(check int) "cycle terminates" 2 (List.length graph.nodes);
  Alcotest.(check int) "the import that closes it is reported" 1
    (List.length graph.diagnostics);
  Alcotest.(check int) "it lands on the file with the back edge" 1
    (List.length (Node.get (Build.diagnostics build (Filename.concat dir "b.rh"))));
  Alcotest.(check int) "and not on the file it points back to" 0
    (List.length (Node.get (Build.diagnostics build root)))

let test_file_diagnostics () =
  let dir = Filename.temp_file "rhodium_file_diagnostics" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let write name content =
    Out_channel.with_open_text (Filename.concat dir name) (fun oc ->
        Out_channel.output_string oc content)
  in
  write "clean.rh" "clean = 1\n";
  write "broken.rh" "broken = (\n";
  write "importer.rh" "import nowhere\n\nimporter = 1\n";
  write "main.rh" "import clean\nimport broken\nimport importer\n\nmain = 1\n";
  let build = Build.create () in
  ignore (Build.discover build ~root:(Filename.concat dir "main.rh"));
  let diagnostics name = Node.get (Build.diagnostics build (Filename.concat dir name)) in
  Alcotest.(check int) "clean file has none" 0 (List.length (diagnostics "clean.rh"));
  Alcotest.(check int) "a syntax error stays in its file" 1
    (List.length (diagnostics "broken.rh"));
  Alcotest.(check int) "an unresolved import lands on the importer" 1
    (List.length (diagnostics "importer.rh"));
  Alcotest.(check int) "the file itself is clean" 0 (List.length (diagnostics "main.rh"));
  Build.set_source build (Filename.concat dir "clean.rh") "clean = (\n";
  Alcotest.(check int) "an edit reaches the diagnostics node" 1
    (List.length (diagnostics "clean.rh"))

let test_deps_unresolved () =
  let dir = Filename.temp_file "rhodium_missing" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Out_channel.with_open_text (Filename.concat dir "main.rh") (fun oc ->
      Out_channel.output_string oc "import nope/missing\n\nmain = 1\n");
  let graph = Build.discover (Build.create ()) ~root:(Filename.concat dir "main.rh") in
  Alcotest.(check int) "reports unresolved import" 1 (List.length graph.diagnostics)

let test_unparseable_file_is_a_node () =
  let dir = Filename.temp_file "rhodium_unparseable" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let write name content =
    Out_channel.with_open_text (Filename.concat dir name) (fun oc ->
        Out_channel.output_string oc content)
  in
  write "main.rh" "import broken\n\nmain = 1\n";
  write "broken.rh" "broken = (\n";
  let build = Build.create () in
  let graph = Build.discover build ~root:(Filename.concat dir "main.rh") in
  Alcotest.(check int) "both files are nodes" 2 (List.length graph.nodes);
  Alcotest.(check (list string)) "and both parse" [ "broken.rh"; "main.rh" ]
    (List.map Filename.basename (List.map (fun (n : Deps.node) -> n.file) graph.nodes)
    |> List.sort compare);
  Alcotest.(check int) "the broken file reports its error once" 1
    (List.length (Node.get (Build.diagnostics build (Filename.concat dir "broken.rh"))))

let test_node_memoizes () =
  let n = Node.create (fun () -> 41) in
  Alcotest.(check int) "value" 41 (Node.get n);
  Alcotest.(check int) "value again" 41 (Node.get n);
  Alcotest.(check int) "executed once" 1 (Node.executions n)

let test_node_recomputes_only_when_a_dependency_changes () =
  let source = Node.input ~eq:Int.equal 1 in
  let mapped = Node.create (fun () -> Node.get source + 1) in
  Alcotest.(check int) "first read" 2 (Node.get mapped);
  Node.set source 1;
  Alcotest.(check int) "same input value" 2 (Node.get mapped);
  Alcotest.(check int) "no recomputation" 1 (Node.executions mapped);
  Node.set source 10;
  Alcotest.(check int) "changed input" 11 (Node.get mapped);
  Alcotest.(check int) "one recomputation" 2 (Node.executions mapped)

let test_node_cutoff_stops_dependents () =
  let source = Node.input ~eq:Int.equal 1 in
  let positive = Node.create ~eq:Bool.equal (fun () -> Node.get source > 0) in
  let answer = Node.create (fun () -> if Node.get positive then "yes" else "no") in
  Alcotest.(check string) "first read" "yes" (Node.get answer);
  Node.set source 5;
  Alcotest.(check string) "same answer" "yes" (Node.get answer);
  Alcotest.(check int) "the middle node recomputed" 2 (Node.executions positive);
  Alcotest.(check int) "its dependent did not" 1 (Node.executions answer)

let with_project files f =
  let dir = Filename.temp_file "rhodium_inc" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  List.iter
    (fun (name, content) ->
      Out_channel.with_open_text (Filename.concat dir name) (fun oc ->
          Out_channel.output_string oc content))
    files;
  f dir

let test_build_reparses_only_changed () =
  with_project
    [ ("main.rh", "import lib\n\nmain = 1\n"); ("lib.rh", "lib = 2\n") ]
    (fun dir ->
      let build = Build.create () in
      let root = Filename.concat dir "main.rh" in
      let lib = Filename.concat dir "lib.rh" in
      ignore (Build.discover build ~root);
      Alcotest.(check int) "both files parsed" 2 (Build.parses build);
      ignore (Build.discover build ~root);
      Alcotest.(check int) "second walk parses nothing" 2 (Build.parses build);
      Build.set_source build lib "lib = 2\n";
      ignore (Build.discover build ~root);
      Alcotest.(check int) "same text is not reparsed" 2 (Build.parses build);
      Build.set_source build lib "lib = 3\n";
      ignore (Build.discover build ~root);
      Alcotest.(check int) "one changed file, one parse" 3 (Build.parses build))

let test_build_keeps_parse_errors_as_values () =
  with_project [ ("main.rh", "main = (\n") ] (fun dir ->
      let build = Build.create () in
      let root = Filename.concat dir "main.rh" in
      let graph = Build.discover build ~root in
      Alcotest.(check bool) "reported" true (graph.Deps.diagnostics <> []);
      let graph = Build.discover build ~root in
      Alcotest.(check bool) "reported again from the memoized failure" true
        (graph.Deps.diagnostics <> []);
      Alcotest.(check int) "parsed once" 1 (Build.parses build))

let () =
  Alcotest.run "rhodium"
    [
      ( "parser",
        [
          Alcotest.test_case "function binding" `Quick test_fn;
          Alcotest.test_case "tuple return" `Quick test_tuple_ret;
          Alcotest.test_case "call with args" `Quick test_call_with_args;
          Alcotest.test_case "apply to never" `Quick test_apply_to_never;
          Alcotest.test_case "bare function value" `Quick test_bare_fn_value;
          Alcotest.test_case "do block" `Quick test_main;
          Alcotest.test_case "ctor parens" `Quick test_ctor_parens;
          Alcotest.test_case "match arms" `Quick test_match;
          Alcotest.test_case "generic type" `Quick test_generic_ty;
          Alcotest.test_case "type decl" `Quick test_type_decl;
          Alcotest.test_case "comments" `Quick test_comments;
          Alcotest.test_case "qualified call" `Quick test_qualified_call;
          Alcotest.test_case "division" `Quick test_division;
        ] );
      ( "binops",
        [
          Alcotest.test_case "mul over add" `Quick test_mul_over_add;
          Alcotest.test_case "parens override" `Quick test_parens_override;
          Alcotest.test_case "left associativity" `Quick test_left_assoc;
          Alcotest.test_case "unary neg" `Quick test_unary_neg;
          Alcotest.test_case "unary not" `Quick test_unary_not;
          Alcotest.test_case "not over and" `Quick test_not_binds_tighter_than_and;
          Alcotest.test_case "and over or" `Quick test_and_over_or;
          Alcotest.test_case "nor" `Quick test_nor;
          Alcotest.test_case "xor" `Quick test_xor;
          Alcotest.test_case "comparison over and" `Quick test_comparison_over_and;
          Alcotest.test_case "equality precedence" `Quick test_equality_precedence;
          Alcotest.test_case "isnt" `Quick test_isnt;
          Alcotest.test_case "isnt precedence" `Quick test_isnt_precedence;
          Alcotest.test_case "is not composes" `Quick test_is_not_is_compositional;
          Alcotest.test_case "isnt keeps ident" `Quick test_isnt_keeps_ident;
          Alcotest.test_case "ctor nullary" `Quick test_ctor_nullary;
          Alcotest.test_case "ctor multi args" `Quick test_ctor_multi_args;
          Alcotest.test_case "ctor arg is call" `Quick test_ctor_arg_is_call;
          Alcotest.test_case "ctor parens disambiguate" `Quick test_ctor_parens_disambiguate;
          Alcotest.test_case "ctor juxtaposition rejected" `Quick test_ctor_juxtaposition_rejected;
          Alcotest.test_case "stmt needs newline" `Quick test_stmt_needs_newline;
          Alcotest.test_case "decl needs newline" `Quick test_decl_needs_newline;
          Alcotest.test_case "mod precedence" `Quick test_mod_precedence;
          Alcotest.test_case "pipe" `Quick test_pipe;
          Alcotest.test_case "nested parens" `Quick test_nested_parens;
          Alcotest.test_case "nonassoc rejected" `Quick test_nonassoc_rejected;
        ] );
      ( "imports",
        [
          Alcotest.test_case "plain" `Quick test_import_plain;
          Alcotest.test_case "glob" `Quick test_import_glob;
          Alcotest.test_case "group" `Quick test_import_group;
          Alcotest.test_case "nested group" `Quick test_import_nested;
        ] );
      ( "deps",
        [
          Alcotest.test_case "resolve and render" `Quick test_deps;
          Alcotest.test_case "cycle is reported" `Quick test_deps_cycle;
          Alcotest.test_case "unresolved import" `Quick test_deps_unresolved;
          Alcotest.test_case "diagnostics are per file" `Quick test_file_diagnostics;
          Alcotest.test_case "unparseable file stays in the graph" `Quick
            test_unparseable_file_is_a_node;
        ] );
      ( "diagnostics",
        [
          Alcotest.test_case "syntax error" `Quick test_syntax_error;
          Alcotest.test_case "lex error" `Quick test_lex_error;
        ] );
      ( "lexer",
        [
          Alcotest.test_case "token positions" `Quick test_token_positions;
          Alcotest.test_case "newline before bar" `Quick test_newline_suppressed_before_bar;
        ] );
      ( "spans",
        [
          Alcotest.test_case "tight subexpressions" `Quick test_spans_are_tight;
          Alcotest.test_case "block spans delimiters" `Quick test_block_spans_cover_delimiters;
        ] );
      ( "incremental",
        [
          Alcotest.test_case "node memoizes" `Quick test_node_memoizes;
          Alcotest.test_case "recomputes on dependency change" `Quick
            test_node_recomputes_only_when_a_dependency_changes;
          Alcotest.test_case "cutoff stops dependents" `Quick test_node_cutoff_stops_dependents;
          Alcotest.test_case "reparses only changed" `Quick test_build_reparses_only_changed;
          Alcotest.test_case "parse errors are values" `Quick
            test_build_keeps_parse_errors_as_values;
        ] );
    ]
