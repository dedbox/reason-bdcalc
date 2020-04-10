/******************************************************************************/
/* Printers and Parsers                                                       */
/******************************************************************************/

open Ast.HExpr;
open Ast.HType;

open Format;

/******************************************************************************/
/* Types                                                                      */
/******************************************************************************/

/* Parse */

let parse_type = str => {
  let lexbuf = Lexing.from_string(str);
  try(Some(Parser.typ_top(Lexer.read, lexbuf))) {
  | Lexer.Error(_)
  | Parser.Error => None
  };
};

/* Print */

let rec pp_type = (fmt, t) =>
  switch (t) {
  | TBool => fprintf(fmt, "Bool")
  | TFun(t1, t2) => fprintf(fmt, "%a -> %a", sub_pp_type, t1, pp_type, t2)
  }

and sub_pp_type = (fmt, t) =>
  switch (t) {
  | TBool => pp_type(fmt, t)
  | TFun(t1, t2) => fprintf(fmt, "(%a -> %a)", sub_pp_type, t1, pp_type, t2)
  };

/******************************************************************************/
/* Expressions                                                                */
/******************************************************************************/

/* Parse */

let parse_expr = str => {
  let lexbuf = Lexing.from_string(str);
  try(Some(Parser.top(Lexer.read, lexbuf))) {
  | Lexer.Error(_)
  | Parser.Error => None
  };
};

/* Print */

let rec pp_expr = (fmt, e) =>
  switch (e) {
  | Var(x) => fprintf(fmt, "%s", x)
  | App(e1, e2) => fprintf(fmt, "%a %a", app_pp_expr, e1, sub_pp_expr, e2)
  | Fun(x, e1) => fprintf(fmt, "\\%s.%a", x, pp_expr, e1)
  | Tru => fprintf(fmt, "true")
  | Fls => fprintf(fmt, "false")
  | If(e1, e2, e3) =>
    let _expr = sub_pp_expr;
    fprintf(fmt, "if %a %a %a", _expr, e1, _expr, e2, _expr, e3);
  | Ann(e1, t2) => fprintf(fmt, "%a : %a", sub_pp_expr, e1, pp_type, t2)
  }

and app_pp_expr = (fmt, e) =>
  switch (e) {
  | Fun(_, _)
  | If(_, _, _)
  | Ann(_, _) => sub_pp_expr(fmt, e)
  | App(e1, e2) => fprintf(fmt, "%a %a", app_pp_expr, e1, sub_pp_expr, e2)
  | _ => pp_expr(fmt, e)
  }

and sub_pp_expr = (fmt, e) =>
  switch (e) {
  | App(_, _)
  | Fun(_, _)
  | If(_, _, _)
  | Ann(_, _) => fprintf(fmt, "(%a)", pp_expr, e)
  | _ => pp_expr(fmt, e)
  };
