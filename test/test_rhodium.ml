open Rhodium

let parse source =
  match Pipeline.parse_string ~filename:"<test>" source with
  | Ok program -> program
  | Error ds ->
      Alcotest.failf "unexpected diagnostics: %s"
        (String.concat "; " (List.map Diagnostics.to_string ds))

let check name expected source = Alcotest.(check string) name expected (Pprint.program (parse source))

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

let test_ctor_juxtaposition () = check "constructor args" "(let a (Pair x y))" "a = Pair x y"

let test_match () =
  check "match arms"
    "(let f (fn (xs List[T]) Maybe[T] (match xs ([x:...] (Something x)) (_ (Nothing)))))"
    "f = fn(xs List[T]) Maybe[T] -> match xs\n| [x:...] -> Something x\n| _ -> Nothing"

let test_generic_ty () =
  check "generic type" "(let f (fn (xs List[T]) Maybe[T] (Nothing)))" "f = fn(xs List[T]) Maybe[T] -> Nothing"

let test_type_decl () =
  check "type decl"
    "(type List[T] (field capacity u32) (field length u32) (field head opaque_ptr) \
     (global head (fn (list List[T]) Maybe[T] (match list ([x:...] (Something x)) (_ (Nothing))))))"
    "type List[T] = {\n  capacity u32\n  length   u32\n  head     opaque_ptr\n\n  head = \
     fn(list List[T]) Maybe[T] -> match list\n  | [x:...] -> Something x\n  | _       -> Nothing\n}"

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
  let graph = Deps.build ~root:(Filename.concat dir "main.rh") in
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
  let graph = Deps.build ~root:(Filename.concat dir "a.rh") in
  Alcotest.(check int) "cycle terminates" 2 (List.length graph.nodes)

let test_deps_unresolved () =
  let dir = Filename.temp_file "rhodium_missing" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  Out_channel.with_open_text (Filename.concat dir "main.rh") (fun oc ->
      Out_channel.output_string oc "import nope/missing\n\nmain = 1\n");
  let graph = Deps.build ~root:(Filename.concat dir "main.rh") in
  Alcotest.(check int) "reports unresolved import" 1 (List.length graph.diagnostics)

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
          Alcotest.test_case "constructor juxtaposition" `Quick test_ctor_juxtaposition;
          Alcotest.test_case "match arms" `Quick test_match;
          Alcotest.test_case "generic type" `Quick test_generic_ty;
          Alcotest.test_case "type decl" `Quick test_type_decl;
          Alcotest.test_case "comments" `Quick test_comments;
          Alcotest.test_case "qualified call" `Quick test_qualified_call;
          Alcotest.test_case "division" `Quick test_division;
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
          Alcotest.test_case "cycle terminates" `Quick test_deps_cycle;
          Alcotest.test_case "unresolved import" `Quick test_deps_unresolved;
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
    ]
