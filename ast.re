/******************************************************************************/
/* Abstract Syntax Trees                                                      */
/******************************************************************************/

module HType = {
  type t =
    | TBool
    | TFun(t, t);
};

module HExpr = {
  type t =
    | Var(string)
    | App(t, t)
    | Fun(string, t)
    | Tru
    | Fls
    | If(t, t, t)
    | Ann(t, HType.t);
};
