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

let test_import () = check "import" "(import std/clang/types/opaque_ptr)" "import std/clang/types/opaque_ptr"

let test_type_decl () =
  check "type decl"
    "(type List[T] (field capacity u32) (field length u32) (field head opaque_ptr) \
     (global head (fn (list List[T]) Maybe[T] (match list ([x:...] (Something x)) (_ (Nothing))))))"
    "type List[T] = {\n  capacity u32\n  length   u32\n  head     opaque_ptr\n\n  head = \
     fn(list List[T]) Maybe[T] -> match list\n  | [x:...] -> Something x\n  | _       -> Nothing\n}"

let test_comments () = check "comments" "(let x 1)" "# leading\nx = 1 # trailing"

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
          Alcotest.test_case "import" `Quick test_import;
          Alcotest.test_case "type decl" `Quick test_type_decl;
          Alcotest.test_case "comments" `Quick test_comments;
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
