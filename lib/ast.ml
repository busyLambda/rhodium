type ty =
  | TName of string * ty list
  | TTuple of ty list

type param = { name : string; ty : ty }

type binop = Add | Div

type expr =
  | EInt of int
  | EUnit
  | EVar of string
  | ECtor of string * expr list
  | ETuple of expr list
  | ECall of expr * expr list
  | EFn of param list * ty * expr
  | EDo of stmt list
  | EMatch of expr * (expr * expr) list
  | EBinop of binop * expr * expr
  | EWildcard
  | EListHead of expr

and stmt = SAssign of expr * expr | SExpr of expr

type member = MField of string * ty | MGlobal of string * expr

type import_sel =
  | SelName of string
  | SelGlob
  | SelGroup of import_sel list
  | SelPath of string * import_sel

type import_decl = { path : string; sel : import_sel option }

type decl =
  | DBind of string * expr
  | DType of string * string list * member list
  | DImport of import_decl

type program = decl list
