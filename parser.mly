%token <string> SYM
%token ARROW COLON
%token BOOL
%token LPAREN RPAREN SLASH DOT
%token TRUE FALSE IF
%token EOF

%right ARROW

%start <HExpr.t> main

%{
    open HExpr;;
    open HType;;
%}

%%

main:
  | expr EOF     { $1 }

expr:
  | SLASH SYM DOT expr  { Fun($2, $4) }
  | IF atom atom atom   { If($2, $3, $4) }
  | appexpr             { $1 }

appexpr:
  | appexpr atom  { App($1, $2) }
  | atom          { $1 }

atom:
  | atom COLON typ      { Ann($1, $3) }
  | SYM                 { Var($1) }
  | TRUE                { Tru }
  | FALSE               { Fls }
  | LPAREN expr RPAREN  { $2 }

typ:
  | BOOL               { TBool }
  | typ ARROW typ      { TFun($1, $3) }
  | LPAREN typ RPAREN  { $2 }
