%token <int> INT
%token <string> IDENT CTOR
%token IMPORT TYPE FN DO END MATCH
%token ARROW EQ BAR COLON COMMA ELLIPSIS UNDERSCORE
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token PLUS SLASH NEWLINE EOF

%start <Ast.program> program

%%

program:
  | ds = decls; EOF { List.rev ds }

decls:
  | { [] }
  | ds = decls; NEWLINE { ds }
  | ds = decls; d = decl { d :: ds }

decl:
  | i = import_decl { i }
  | t = type_decl { t }
  | x = IDENT; EQ; e = expr { Ast.DBind (x, e) }

import_decl:
  | IMPORT; p = path { Ast.DImport p }

path:
  | x = IDENT { x }
  | p = path; SLASH; x = IDENT { p ^ "/" ^ x }

type_decl:
  | TYPE; n = name; ps = type_params; EQ; ms = record { Ast.DType (n, ps, ms) }

type_params:
  | { [] }
  | LBRACK; ps = separated_list(COMMA, name); RBRACK { ps }

record:
  | LBRACE; ms = members; RBRACE { List.rev ms }

members:
  | { [] }
  | ms = members; NEWLINE { ms }
  | ms = members; m = member { m :: ms }

member:
  | n = name; t = ty { Ast.MField (n, t) }
  | n = name; EQ; e = expr { Ast.MGlobal (n, e) }

expr:
  | e = expr; PLUS; a = atom { Ast.EBinop (Ast.Add, e, a) }
  | a = atom { a }

atom:
  | n = INT { Ast.EInt n }
  | LPAREN; RPAREN { Ast.EUnit }
  | LPAREN; e = expr; RPAREN { e }
  | x = IDENT; args = args { Ast.ECall (Ast.EVar x, args) }
  | x = IDENT { Ast.EVar x }
  | c = CTOR; args = list(atom) { Ast.ECtor (c, args) }
  | LBRACE; es = separated_list(COMMA, expr); RBRACE { Ast.ETuple es }
  | UNDERSCORE { Ast.EWildcard }
  | LBRACK; e = expr; COLON; ELLIPSIS; RBRACK { Ast.EListHead e }
  | f = fn_expr { f }
  | d = do_block { d }
  | m = match_expr { m }

args:
  | LPAREN; es = separated_list(COMMA, expr); RPAREN { es }

fn_expr:
  | FN; LPAREN; ps = separated_list(COMMA, param); RPAREN; r = ty; ARROW; b = expr
      { Ast.EFn (ps, r, b) }

param:
  | x = IDENT; t = ty { { Ast.name = x; ty = t } }

do_block:
  | DO; ss = stmts; END { Ast.EDo (List.rev ss) }

stmts:
  | { [] }
  | ss = stmts; NEWLINE { ss }
  | ss = stmts; s = stmt { s :: ss }

stmt:
  | p = expr; EQ; e = expr { Ast.SAssign (p, e) }
  | e = expr { Ast.SExpr e }

match_expr:
  | MATCH; e = expr; arms = arms { Ast.EMatch (e, List.rev arms) }

arms:
  | a = arm { [ a ] }
  | arms = arms; a = arm { a :: arms }

arm:
  | BAR; p = expr; ARROW; b = expr { (p, b) }

ty:
  | n = name; args = ty_args { Ast.TName (n, args) }
  | LBRACE; ts = separated_list(COMMA, ty); RBRACE { Ast.TTuple ts }

ty_args:
  | { [] }
  | LBRACK; ts = separated_list(COMMA, ty); RBRACK { ts }

name:
  | x = IDENT { x }
  | x = CTOR { x }
