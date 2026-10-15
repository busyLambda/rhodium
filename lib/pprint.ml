let app head args = (Printf.sprintf : ('a, unit, string) format -> 'a) "(%s%s)" head (String.concat "" (List.map (fun a -> " " ^ a) args))

let rec ty = function
  | Ast.TName (n, []) -> n
  | Ast.TName (n, args) -> Printf.sprintf "%s[%s]" n (String.concat ", " (List.map ty args))
  | Ast.TTuple ts -> Printf.sprintf "{%s}" (String.concat ", " (List.map ty ts))

let rec expr = function
  | Ast.EInt n -> string_of_int n
  | Ast.EUnit -> "()"
  | Ast.EVar x -> x
  | Ast.ETuple es -> Printf.sprintf "{%s}" (String.concat ", " (List.map expr es))
  | Ast.ECtor (c, es) -> app c (List.map expr es)
  | Ast.ECall (f, args) -> app (expr f) (List.map expr args)
  | Ast.EFn (ps, r, b) ->
      Printf.sprintf "(fn (%s) %s %s)" (String.concat ", " (List.map param ps)) (ty r) (expr b)
  | Ast.EDo ss -> Printf.sprintf "(do %s)" (String.concat " " (List.map stmt ss))
  | Ast.EMatch (e, arms) ->
      Printf.sprintf "(match %s %s)" (expr e)
        (String.concat " "
           (List.map (fun (p, b) -> Printf.sprintf "(%s %s)" (expr p) (expr b)) arms))
  | Ast.EBinop (Ast.Add, a, b) -> Printf.sprintf "(+ %s %s)" (expr a) (expr b)
  | Ast.EWildcard -> "_"
  | Ast.EListHead e -> Printf.sprintf "[%s:...]" (expr e)

and param (p : Ast.param) = Printf.sprintf "%s %s" p.name (ty p.ty)

and stmt = function
  | Ast.SAssign (p, e) -> Printf.sprintf "(= %s %s)" (expr p) (expr e)
  | Ast.SExpr e -> expr e

let member = function
  | Ast.MField (n, t) -> Printf.sprintf "(field %s %s)" n (ty t)
  | Ast.MGlobal (n, e) -> Printf.sprintf "(global %s %s)" n (expr e)

let decl = function
  | Ast.DBind (x, e) -> Printf.sprintf "(let %s %s)" x (expr e)
  | Ast.DType (n, ps, ms) ->
      Printf.sprintf "(type %s[%s] %s)" n (String.concat ", " ps)
        (String.concat " " (List.map member ms))
  | Ast.DImport p -> Printf.sprintf "(import %s)" p

let program ds = String.concat "\n" (List.map decl ds)
