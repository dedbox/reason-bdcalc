/******************************************************************************/
/* Types                                                                      */
/******************************************************************************/

type t =
  | TBool
  | TFun(t, t);

open Format;

let rec pp = (fmt, t) =>
  switch (t) {
  | TBool => fprintf(fmt, "Bool")
  | TFun(t1, t2) => fprintf(fmt, "%a -> %a", sub_pp, t1, pp, t2)
  }

and sub_pp = (fmt, t) =>
  switch (t) {
  | TBool => pp(fmt, t)
  | TFun(t1, t2) => fprintf(fmt, "(%a -> %a)", sub_pp, t1, pp, t2)
  };
