type span = Loc.t

type ty = { desc : ty_desc; span : span }
and ty_desc = TName of string * ty list | TTuple of ty list

type param = { name : string; ty : ty; span : span }

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
  | EFn of param list * ty * expr
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
  | DType of string * string list * member list
  | DImport of import_decl

type program = decl list

let mk_ty span desc : ty = { desc; span }
let mk_param span name ty : param = { name; ty; span }
let mk_expr span desc : expr = { desc; span }
let mk_stmt span desc : stmt = { desc; span }
let mk_arm span pattern body : arm = { pattern; body; span }
let mk_member span desc : member = { desc; span }
let mk_sel span desc : import_sel = { desc; span }
let mk_import span path sel : import_decl = { path; sel; span }
let mk_decl span desc : decl = { desc; span }
