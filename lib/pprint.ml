let app head args =
  Printf.sprintf "(%s%s)" head (String.concat "" (List.map (fun a -> " " ^ a) args))

let binop_to_string = function
  | Ast.Pipe -> "|>"
  | Ast.Nor -> "nor"
  | Ast.Or -> "or"
  | Ast.Xor -> "xor"
  | Ast.And -> "and"
  | Ast.Eq -> "is"
  | Ast.Neq -> "isnt"
  | Ast.Lt -> "<"
  | Ast.Le -> "<="
  | Ast.Gt -> ">"
  | Ast.Ge -> ">="
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Mod -> "%"

let unop_to_string = function Ast.Neg -> "-" | Ast.Not -> "not"

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
  | Ast.EBinop (op, a, b) -> Printf.sprintf "(%s %s %s)" (binop_to_string op) (expr a) (expr b)
  | Ast.EUnop (op, a) -> Printf.sprintf "(%s %s)" (unop_to_string op) (expr a)
  | Ast.EWildcard -> "_"
  | Ast.EListHead e -> Printf.sprintf "[%s:...]" (expr e)

and param (p : Ast.param) = Printf.sprintf "%s %s" p.name (ty p.ty)

and stmt = function
  | Ast.SAssign (p, e) -> Printf.sprintf "(= %s %s)" (expr p) (expr e)
  | Ast.SExpr e -> expr e

let member = function
  | Ast.MField (n, t) -> Printf.sprintf "(field %s %s)" n (ty t)
  | Ast.MGlobal (n, e) -> Printf.sprintf "(global %s %s)" n (expr e)

let rec sel = function
  | Ast.SelName x -> x
  | Ast.SelGlob -> "*"
  | Ast.SelGroup items -> Printf.sprintf "{%s}" (String.concat ", " (List.map sel items))
  | Ast.SelPath (x, s) -> Printf.sprintf "%s/%s" x (sel s)

let import (i : Ast.import_decl) =
  match i.sel with
  | None -> Printf.sprintf "(import %s)" i.path
  | Some Ast.SelGlob -> Printf.sprintf "(import %s/*)" i.path
  | Some s -> Printf.sprintf "(import %s/%s)" i.path (sel s)

let decl = function
  | Ast.DBind (x, e) -> Printf.sprintf "(let %s %s)" x (expr e)
  | Ast.DType (n, ps, ms) ->
      Printf.sprintf "(type %s[%s] %s)" n (String.concat ", " ps)
        (String.concat " " (List.map member ms))
  | Ast.DImport i -> import i

let program ds = String.concat "\n" (List.map decl ds)
