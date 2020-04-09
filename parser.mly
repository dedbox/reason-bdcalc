%token <string> SYM
%token ARROW COLON
%token BOOL
%token LPAREN RPAREN SLASH DOT
%token TRUE FALSE IF
%token EOF

%right ARROW

%start <HExpr.t> top
%start <HType.t> typ_top

%{
    open HExpr;;
    open HType;;

%}

%%

top:
  | e=expr EOF  { e }

typ_top:
  | t=typ EOF  { t }

expr:
  | SLASH x=SYM DOT e=expr      { Fun(x, e) }
  | IF e1=atom e2=atom e3=atom  { If(e1, e2, e3) }
  | appexpr                     { $1 }

appexpr:
  | e1=appexpr e2=atom  { App(e1, e2) }
  | atom                { $1 }

atom:
  | e=atom COLON t=typ  { Ann(e, t) }
  | x=SYM               { Var(x) }
  | TRUE                { Tru }
  | FALSE               { Fls }
  | LPAREN expr RPAREN  { $2 }

typ:
  | BOOL                 { TBool }
  | t1=typ ARROW t2=typ  { TFun(t1, t2) }
  | LPAREN typ RPAREN    { $2 }
