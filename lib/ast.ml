type span = Loc.t

type ty = { desc : ty_desc; span : span }
and ty_desc =
  | TName of string * ty list
  | TTuple of ty list
  (* [*T] and [*mut T]: raw pointers, the untraced handles tracked by nobody. *)
  | TPtr of ty
  | TMutPtr of ty | TFn of ty list * ty

type param = { name : string; ty : ty; span : span }

(* A type parameter of a type declaration or a lambda, with the contracts it must satisfy. *)
type type_param = { name : string; bounds : ty list; span : span }

(* One alternative of a sum declaration; [args] is its payload. *)
type variant = { name : string; args : ty list; span : span }

type binop =
  | Pipe
  | Nor
  | Or
  | Xor
  | And
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type unop = Neg | Not

type expr = { desc : expr_desc; span : span }

and expr_desc =
  | EInt of int
  | EUnit
  | EVar of string
  | ECtor of string * expr list
  | ETuple of expr list
  | ECall of expr * expr list
  | EField of expr * string
  | EFn of type_param list * param list * ty * expr
  | EDo of stmt list
  | EMatch of expr * arm list
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | EWildcard
  | EListHead of expr

and stmt = { desc : stmt_desc; span : span }
and stmt_desc = SAssign of expr * expr | SExpr of expr

and arm = { pattern : expr; body : expr; span : span }

type member = { desc : member_desc; span : span }
and member_desc = MField of string * ty | MGlobal of string * expr

type import_sel = { desc : sel_desc; span : span }

and sel_desc =
  | SelName of string
  | SelGlob
  | SelGroup of import_sel list
  | SelPath of string * import_sel

type import_decl = { path : string; sel : import_sel option; span : span }

type decl = { desc : decl_desc; span : span }

and decl_desc =
  | DBind of string * expr
  (* name, type parameters, shape fields, associated items *)
  | DType of string * type_param list * member list * member list
  (* name, type parameters, variants, associated items *)
  | DSum of string * type_param list * variant list * member list
  (* name, type parameters, representation *)
  | DNewtype of string * type_param list * ty
  | DImport of import_decl

type program = decl list

let mk_ty span desc : ty = { desc; span }
let mk_param span name ty : param = { name; ty; span }
let mk_type_param span name bounds : type_param = { name; bounds; span }
let mk_variant span name args : variant = { name; args; span }
let mk_expr span desc : expr = { desc; span }
let mk_stmt span desc : stmt = { desc; span }
let mk_arm span pattern body : arm = { pattern; body; span }
let mk_member span desc : member = { desc; span }
let mk_sel span desc : import_sel = { desc; span }
let mk_import span path sel : import_decl = { path; sel; span }
let mk_decl span desc : decl = { desc; span }
