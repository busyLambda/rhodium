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

let rec ty (t : Ast.ty) =
  match t.desc with
  | Ast.TName (n, []) -> n
  | Ast.TName (n, args) -> Printf.sprintf "%s[%s]" n (String.concat ", " (List.map ty args))
  | Ast.TFn (ps, r) -> Printf.sprintf "fn(%s) %s" (String.concat ", " (List.map ty ps)) (ty r)
  | Ast.TTuple ts -> Printf.sprintf "{%s}" (String.concat ", " (List.map ty ts))
  | Ast.TPtr t -> Printf.sprintf "(ptr %s)" (ty t)
  | Ast.TMutPtr t -> Printf.sprintf "(mut-ptr %s)" (ty t)

let rec expr (e : Ast.expr) =
  match e.desc with
  | Ast.EInt n -> string_of_int n
  | Ast.EUnit -> "()"
  | Ast.EVar x -> x
  | Ast.ETuple es -> Printf.sprintf "{%s}" (String.concat ", " (List.map expr es))
  | Ast.ECtor (c, es) -> app c (List.map expr es)
  | Ast.ECall (f, args) -> app (expr f) (List.map expr args)
  | Ast.EField (e, n) -> Printf.sprintf "(. %s %s)" (expr e) n
  | Ast.EFn (tps, ps, r, b) ->
      let generics = match tps with [] -> "" | _ -> type_params tps in
      Printf.sprintf "(fn%s (%s) %s %s)" generics
        (String.concat ", " (List.map param ps)) (ty r) (expr b)
  | Ast.EDo ss -> Printf.sprintf "(do %s)" (String.concat " " (List.map stmt ss))
  | Ast.EMatch (e, arms) ->
      Printf.sprintf "(match %s %s)" (expr e) (String.concat " " (List.map arm arms))
  | Ast.EBinop (op, a, b) -> Printf.sprintf "(%s %s %s)" (binop_to_string op) (expr a) (expr b)
  | Ast.EUnop (op, a) -> Printf.sprintf "(%s %s)" (unop_to_string op) (expr a)
  | Ast.EWildcard -> "_"
  | Ast.EListHead e -> Printf.sprintf "[%s:...]" (expr e)

and param (p : Ast.param) = Printf.sprintf "%s %s" p.name (ty p.ty)

and type_param (p : Ast.type_param) =
  match p.bounds with
  | [] -> p.name
  | bs -> Printf.sprintf "%s: %s" p.name (String.concat " + " (List.map ty bs))

and type_params ps = "[" ^ String.concat ", " (List.map type_param ps) ^ "]"

and stmt (s : Ast.stmt) =
  match s.desc with
  | Ast.SAssign (p, e) -> Printf.sprintf "(= %s %s)" (expr p) (expr e)
  | Ast.SExpr e -> expr e

and arm (a : Ast.arm) = Printf.sprintf "(%s %s)" (expr a.pattern) (expr a.body)

let variant (v : Ast.variant) = app ("variant " ^ v.name) (List.map ty v.args)

let member (m : Ast.member) =
  match m.desc with
  | Ast.MField (n, t) -> Printf.sprintf "(field %s %s)" n (ty t)
  | Ast.MGlobal (n, e) -> Printf.sprintf "(global %s %s)" n (expr e)

let associated ws =
  match ws with
  | [] -> []
  | ws -> [ Printf.sprintf "(where %s)" (String.concat " " (List.map member ws)) ]

let rec sel (s : Ast.import_sel) =
  match s.desc with
  | Ast.SelName x -> x
  | Ast.SelGlob -> "*"
  | Ast.SelGroup items -> Printf.sprintf "{%s}" (String.concat ", " (List.map sel items))
  | Ast.SelPath (x, sub) -> Printf.sprintf "%s/%s" x (sel sub)

let import (i : Ast.import_decl) =
  match i.sel with
  | None -> Printf.sprintf "(import %s)" i.path
  | Some { Ast.desc = Ast.SelGlob; _ } -> Printf.sprintf "(import %s/*)" i.path
  | Some s -> Printf.sprintf "(import %s/%s)" i.path (sel s)

let decl (d : Ast.decl) =
  match d.desc with
  | Ast.DBind (x, e) -> Printf.sprintf "(let %s %s)" x (expr e)
  | Ast.DType (n, ps, fs, ws) ->
      Printf.sprintf "(type %s%s %s)" n (type_params ps)
        (String.concat " " (List.map member fs @ associated ws))
  | Ast.DSum (n, ps, vs, ws) ->
      Printf.sprintf "(sum %s%s %s)" n (type_params ps)
        (String.concat " " (List.map variant vs @ associated ws))
  | Ast.DNewtype (n, ps, t) ->
      Printf.sprintf "(newtype %s%s %s)" n (type_params ps) (ty t)
  | Ast.DImport i -> import i

let program ds = String.concat "\n" (List.map decl ds)
