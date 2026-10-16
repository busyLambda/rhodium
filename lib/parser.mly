%token <int> INT
%token <string> IDENT CTOR
%token IMPORT TYPE FN DO END MATCH
%token ARROW EQ BAR COLON COLONCOLON COMMA ELLIPSIS UNDERSCORE STAR
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
  | IMPORT; p = module_path { Ast.DImport { path = p; sel = None } }
  | IMPORT; p = module_path; SLASH; STAR { Ast.DImport { path = p; sel = Some Ast.SelGlob } }
  | IMPORT; p = module_path; SLASH; g = sel_group
      { Ast.DImport { path = p; sel = Some g } }

module_path:
  | x = name { x }
  | p = module_path; SLASH; x = name { p ^ "/" ^ x }

sel_group:
  | LBRACE; items = separated_list(COMMA, sel_item); RBRACE { Ast.SelGroup items }

sel_item:
  | x = name { Ast.SelName x }
  | STAR { Ast.SelGlob }
  | g = sel_group { g }
  | x = name; SLASH; s = sel_item { Ast.SelPath (x, s) }

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
  | e = expr; PLUS; t = term { Ast.EBinop (Ast.Add, e, t) }
  | t = term { t }

term:
  | t = term; SLASH; a = atom { Ast.EBinop (Ast.Div, t, a) }
  | a = atom { a }

atom:
  | n = INT { Ast.EInt n }
  | LPAREN; RPAREN { Ast.EUnit }
  | LPAREN; e = expr; RPAREN { e }
  | x = IDENT; args = args { Ast.ECall (Ast.EVar x, args) }
  | x = IDENT { Ast.EVar x }
  | x = IDENT; COLONCOLON; rest = qname; args = args
      { Ast.ECall (Ast.EVar (x ^ "::" ^ rest), args) }
  | x = IDENT; COLONCOLON; rest = qname { Ast.EVar (x ^ "::" ^ rest) }
  | c = CTOR; args = list(atom) { Ast.ECtor (c, args) }
  | c = CTOR; COLONCOLON; rest = qname; args = args
      { Ast.ECall (Ast.EVar (c ^ "::" ^ rest), args) }
  | c = CTOR; COLONCOLON; rest = qname { Ast.EVar (c ^ "::" ^ rest) }
  | LBRACE; es = separated_list(COMMA, expr); RBRACE { Ast.ETuple es }
  | UNDERSCORE { Ast.EWildcard }
  | LBRACK; e = expr; COLON; ELLIPSIS; RBRACK { Ast.EListHead e }
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
