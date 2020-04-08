/******************************************************************************/
/* Expressions                                                                */
/******************************************************************************/

type t =
  | Var(string)
  | App(t, t)
  | Fun(string, t)
  | Tru
  | Fls
  | If(t, t, t)
  | Ann(t, HType.t);

open Format;

let rec pp = (fmt, e) =>
  switch (e) {
  | Var(x) => fprintf(fmt, "%s", x)
  | App(e1, e2) => fprintf(fmt, "%a %a", app_pp, e1, sub_pp, e2)
  | Fun(x, e1) => fprintf(fmt, "\\%s.%a", x, pp, e1)
  | Tru => fprintf(fmt, "true")
  | Fls => fprintf(fmt, "false")
  | If(e1, e2, e3) =>
    fprintf(fmt, "if %a %a %a", sub_pp, e1, sub_pp, e2, sub_pp, e3)
  | Ann(e1, t2) => fprintf(fmt, "%a : %a", sub_pp, e1, HType.pp, t2)
  }

and app_pp = (fmt, e) =>
  switch (e) {
  | Fun(_, _)
  | If(_, _, _)
  | Ann(_, _) => sub_pp(fmt, e)
  | App(e1, e2) => fprintf(fmt, "%a %a", app_pp, e1, sub_pp, e2)
  | _ => pp(fmt, e)
  }

and sub_pp = (fmt, e) =>
  switch (e) {
  | App(_, _)
  | Fun(_, _)
  | If(_, _, _)
  | Ann(_, _) => fprintf(fmt, "(%a)", pp, e)
  | _ => pp(fmt, e)
  };
