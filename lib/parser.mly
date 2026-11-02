%{
let sp = Loc.of_lexing
%}

%token <int> INT
%token <string> IDENT CTOR
%token IMPORT TYPE FN DO END MATCH
%token NOT AND OR NOR XOR IS ISNT
%token ARROW EQ BAR COLON COLONCOLON COMMA ELLIPSIS UNDERSCORE STAR
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
  | TYPE; n = name; ps = type_params; EQ; ms = record
      { Ast.mk_decl (sp $startpos $endpos) (Ast.DType (n, ps, ms)) }
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
  | LBRACK; ps = separated_list(COMMA, name); RBRACK { ps }

record:
  | LBRACE; ms = members; RBRACE { List.rev ms }

members:
  | { [] }
  | ms = members; NEWLINE { ms }
  | m = member { [ m ] }
  | ms = members; NEWLINE; m = member { m :: ms }

member:
  | n = name; t = ty { Ast.mk_member (sp $startpos $endpos) (Ast.MField (n, t)) }
  | n = name; EQ; e = expr { Ast.mk_member (sp $startpos $endpos) (Ast.MGlobal (n, e)) }

expr:
  | e1 = expr; op = binop; e2 = expr
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EBinop (op, e1, e2)) }
  | a = atom { a }

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
  | MINUS; a = atom { Ast.mk_expr (sp $startpos $endpos) (Ast.EUnop (Ast.Neg, a)) }
  | NOT; a = atom { Ast.mk_expr (sp $startpos $endpos) (Ast.EUnop (Ast.Not, a)) }
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
  | f = fn_expr { f }
  | d = do_block { d }
  | m = match_expr { m }

qname:
  | x = name { x }
  | q = qname; COLONCOLON; x = name { q ^ "::" ^ x }

args:
  | LPAREN; es = separated_list(COMMA, expr); RPAREN { es }

fn_expr:
  | FN; LPAREN; ps = separated_list(COMMA, param); RPAREN; r = ty; ARROW; b = expr
      { Ast.mk_expr (sp $startpos $endpos) (Ast.EFn (ps, r, b)) }

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
  | LBRACE; ts = separated_list(COMMA, ty); RBRACE
      { Ast.mk_ty (sp $startpos $endpos) (Ast.TTuple ts) }

ty_args:
  | { [] }
  | LBRACK; ts = separated_list(COMMA, ty); RBRACK { ts }

name:
  | x = IDENT { x }
  | x = CTOR { x }
