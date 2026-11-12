%{
let sp = Loc.of_lexing
%}

%token <int> INT
%token <string> IDENT CTOR
%token IMPORT TYPE FN DO END MATCH WHERE MUT
%token NOT AND OR NOR XOR IS ISNT
%token ARROW EQ BAR COLON COLONCOLON COMMA ELLIPSIS DOT UNDERSCORE STAR
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PIPE LT LE GT GE PLUS MINUS SLASH PERCENT
%token NEWLINE EOF

%left PIPE
%left NOR
%left OR XOR
%left AND
%nonassoc IS ISNT
%nonassoc LT LE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT

%start <Ast.program> program

%%

program:
  | ds = decls; EOF { List.rev ds }

decls:
  | { [] }
  | ds = decls; NEWLINE { ds }
  | d = decl { [ d ] }
  | ds = decls; NEWLINE; d = decl { d :: ds }

decl:
  | i = import_decl { Ast.mk_decl (sp $startpos $endpos) (Ast.DImport i) }
  | TYPE; n = name; ps = type_params; EQ; ms = record; ws = where_block
      { Ast.mk_decl (sp $startpos $endpos) (Ast.DType (n, ps, ms, ws)) }
  | TYPE; n = name; ps = type_params; EQ; vs = variants; ws = where_block
      { Ast.mk_decl (sp $startpos $endpos) (Ast.DSum (n, ps, List.rev vs, ws)) }
  | TYPE; n = name; ps = type_params; EQ; t = repr_ty
      { Ast.mk_decl (sp $startpos $endpos) (Ast.DNewtype (n, ps, t)) }
  | x = IDENT; EQ; e = expr { Ast.mk_decl (sp $startpos $endpos) (Ast.DBind (x, e)) }

import_decl:
  | IMPORT; p = module_path { Ast.mk_import (sp $startpos $endpos) p None }
  | IMPORT; p = module_path; SLASH; _star = STAR
      { Ast.mk_import (sp $startpos $endpos) p
          (Some (Ast.mk_sel (sp $startpos(_star) $endpos(_star)) Ast.SelGlob)) }
  | IMPORT; p = module_path; SLASH; g = sel_group
      { Ast.mk_import (sp $startpos $endpos) p (Some g) }

module_path:
  | x = name { x }
  | p = module_path; SLASH; x = name { p ^ "/" ^ x }

sel_group:
  | LBRACE; items = separated_list(COMMA, sel_item); RBRACE
      { Ast.mk_sel (sp $startpos $endpos) (Ast.SelGroup items) }

sel_item:
  | x = name { Ast.mk_sel (sp $startpos $endpos) (Ast.SelName x) }
  | STAR { Ast.mk_sel (sp $startpos $endpos) Ast.SelGlob }
  | g = sel_group { g }
  | x = name; SLASH; s = sel_item
      { Ast.mk_sel (sp $startpos $endpos) (Ast.SelPath (x, s)) }

type_params:
  | { [] }
  | LBRACK; ps = separated_list(COMMA, type_param); RBRACK { ps }

type_param:
  | n = name; bs = bounds { Ast.mk_type_param (sp $startpos $endpos) n bs }

bounds:
  | { [] }
  | COLON; bs = separated_list(PLUS, ty) { bs }

record:
  | LBRACE; fs = fields; RBRACE { List.rev fs }

fields:
  | { [] }
  | fs = fields; NEWLINE { fs }
  | f = field { [ f ] }
  | fs = fields; NEWLINE; f = field { f :: fs }

field:
  | n = name; t = ty { Ast.mk_member (sp $startpos $endpos) (Ast.MField (n, t)) }

(* A nominal newtype's representation. Deliberately not [ty]: a leading brace
   would be a record body, and [ty]'s brace form is ambiguous with it. *)
repr_ty:
  | n = name; args = ty_args { Ast.mk_ty (sp $startpos $endpos) (Ast.TName (n, args)) }

(* The lexer drops the newline before a [|] and before [where], so arms and
   variants need no separator of their own and the where section may start on
   the following line. *)
variants:
  | v = variant { [ v ] }
  | vs = variants; v = variant { v :: vs }

variant:
  | BAR; n = name { Ast.mk_variant (sp $startpos $endpos) n [] }
  | BAR; n = name; LPAREN; ts = separated_list(COMMA, ty); RPAREN
      { Ast.mk_variant (sp $startpos $endpos) n ts }

(* The braces after the name are the shape; associated items live here. An
   associated item's first parameter is the receiver, so no [self] is needed. *)
where_block:
  | { [] }
  | WHERE; LBRACE; gs = associated; RBRACE { List.rev gs }

associated:
  | { [] }
  | gs = associated; NEWLINE { gs }
  | g = associated_item { [ g ] }
  | gs = associated; NEWLINE; g = associated_item { g :: gs }

associated_item:
  | n = name; EQ; e = expr { Ast.mk_member (sp $startpos $endpos) (Ast.MGlobal (n, e)) }

expr:
  | e1 = expr; op = binop; e2 = expr
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EBinop (op, e1, e2)) }
  | u = unary { u }
  | f = fn_expr { f }
  | m = match_expr { m }

(* Unary binds looser than field access: [-a.b] is [(- (. a b))]. fn and match
   literals are expr-level rather than atoms, so a [.] applied to one needs
   parentheses: [(fn() i32 -> 1).f]. *)
unary:
  | NOT; a = unary { Ast.mk_expr (sp $startpos $endpos) (Ast.EUnop (Ast.Not, a)) }
  | MINUS; a = unary { Ast.mk_expr (sp $startpos $endpos) (Ast.EUnop (Ast.Neg, a)) }
  | p = postfix { p }

(* Field access binds tighter than application: [f(x).y] is [(f x).y]. *)
postfix:
  | a = atom { a }
  | e = postfix; DOT; n = name
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EField (e, n)) }

%inline binop:
  | PIPE { Ast.Pipe }
  | NOR { Ast.Nor }
  | OR { Ast.Or }
  | XOR { Ast.Xor }
  | AND { Ast.And }
  | IS { Ast.Eq }
  | ISNT { Ast.Neq }
  | LT { Ast.Lt }
  | LE { Ast.Le }
  | GT { Ast.Gt }
  | GE { Ast.Ge }
  | PLUS { Ast.Add }
  | MINUS { Ast.Sub }
  | STAR { Ast.Mul }
  | SLASH { Ast.Div }
  | PERCENT { Ast.Mod }

atom:
  | n = INT { Ast.mk_expr (sp $startpos $endpos) (Ast.EInt n) }
  | LPAREN; RPAREN { Ast.mk_expr (sp $startpos $endpos) Ast.EUnit }
  | LPAREN; e = expr; RPAREN { e }
  | x = IDENT; args = args
      { Ast.mk_expr (sp $startpos $endpos)
          (Ast.ECall (Ast.mk_expr (sp $startpos(x) $endpos(x)) (Ast.EVar x), args)) }
  | x = IDENT { Ast.mk_expr (sp $startpos $endpos) (Ast.EVar x) }
  | x = IDENT; COLONCOLON; rest = qname; args = args
      { Ast.mk_expr (sp $startpos $endpos)
          (Ast.ECall
             ( Ast.mk_expr (sp $startpos(x) $endpos(rest)) (Ast.EVar (x ^ "::" ^ rest)),
               args )) }
  | x = IDENT; COLONCOLON; rest = qname
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EVar (x ^ "::" ^ rest)) }
  | c = CTOR; args = args
      { Ast.mk_expr (sp $startpos $endpos) (Ast.ECtor (c, args)) }
  | c = CTOR { Ast.mk_expr (sp $startpos $endpos) (Ast.ECtor (c, [])) }
  | c = CTOR; COLONCOLON; rest = qname; args = args
      { Ast.mk_expr (sp $startpos $endpos)
          (Ast.ECall
             ( Ast.mk_expr (sp $startpos(c) $endpos(rest)) (Ast.EVar (c ^ "::" ^ rest)),
               args )) }
  | c = CTOR; COLONCOLON; rest = qname
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EVar (c ^ "::" ^ rest)) }
  | LBRACE; es = separated_list(COMMA, expr); RBRACE
      { Ast.mk_expr (sp $startpos $endpos) (Ast.ETuple es) }
  | UNDERSCORE { Ast.mk_expr (sp $startpos $endpos) Ast.EWildcard }
  | LBRACK; e = expr; COLON; ELLIPSIS; RBRACK
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EListHead e) }
  | d = do_block { d }

qname:
  | x = name { x }
  | q = qname; COLONCOLON; x = name { q ^ "::" ^ x }

args:
  | LPAREN; es = separated_list(COMMA, expr); RPAREN { es }

fn_expr:
  | FN; tps = type_params; LPAREN; ps = separated_list(COMMA, param); RPAREN; r = ty; ARROW;
    b = expr
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EFn (tps, ps, r, b)) }

param:
  | x = IDENT; t = ty { Ast.mk_param (sp $startpos $endpos) x t }

do_block:
  | DO; ss = stmts; END { Ast.mk_expr (sp $startpos $endpos) (Ast.EDo (List.rev ss)) }

stmts:
  | { [] }
  | ss = stmts; NEWLINE { ss }
  | s = stmt { [ s ] }
  | ss = stmts; NEWLINE; s = stmt { s :: ss }

stmt:
  | p = expr; EQ; e = expr { Ast.mk_stmt (sp $startpos $endpos) (Ast.SAssign (p, e)) }
  | e = expr { Ast.mk_stmt (sp $startpos $endpos) (Ast.SExpr e) }

match_expr:
  | MATCH; e = expr; arms = arms
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EMatch (e, List.rev arms)) }

arms:
  | a = arm { [ a ] }
  | arms = arms; a = arm { a :: arms }

arm:
  | BAR; p = expr; ARROW; b = expr { Ast.mk_arm (sp $startpos $endpos) p b }

ty:
  | n = name; args = ty_args { Ast.mk_ty (sp $startpos $endpos) (Ast.TName (n, args)) }
  | FN; LPAREN; ts = separated_list(COMMA, ty); RPAREN; r = ty
      { Ast.mk_ty (sp $startpos $endpos) (Ast.TFn (ts, r)) }
  | LBRACE; ts = separated_list(COMMA, ty); RBRACE
      { Ast.mk_ty (sp $startpos $endpos) (Ast.TTuple ts) }
  | STAR; t = ty { Ast.mk_ty (sp $startpos $endpos) (Ast.TPtr t) }
  | STAR; MUT; t = ty { Ast.mk_ty (sp $startpos $endpos) (Ast.TMutPtr t) }

ty_args:
  | { [] }
  | LBRACK; ts = separated_list(COMMA, ty); RBRACK { ts }

name:
  | x = IDENT { x }
  | x = CTOR { x }
