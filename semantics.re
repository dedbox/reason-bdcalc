/******************************************************************************/
/* Static Semantics                                                           */
/******************************************************************************/

open Ast.HExpr;
open Ast.HType;

/******************************************************************************/
/* Typing Contexts                                                            */
/******************************************************************************/

module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

let lookup = (x, ctx) =>
  ctx |> StringMap.mem(x) ? Some(ctx |> StringMap.find(x)) : None;

let extend = (x, t, ctx) => ctx |> StringMap.add(x, t);

/******************************************************************************/
/* Bidirectional Type Checking                                                */
/******************************************************************************/

let rec inferType = (e, ctx) =>
  switch (e) {
  | Var(x) => ctx |> lookup(x)
  | Tru
  | Fls => Some(TBool)
  | Ann(e, t) => ctx |> checkType(e, t)
  | App(e1, e2) =>
    switch (ctx |> inferType(e1)) {
    | Some(TFun(t1, t2)) =>
      switch (ctx |> checkType(e2, t1)) {
      | Some(_) => Some(t2)
      | None => None
      }
    | _ => None
    }
  | _ => None
  }

and checkType = (e, t, ctx) =>
  switch (e) {
  | Fun(x1, e2) =>
    switch (t) {
    | TFun(t1, t2) =>
      switch (ctx |> extend(x1, t1) |> checkType(e2, t2)) {
      | Some(_) => Some(t)
      | _ => None
      }
    | _ => None
    }
  | If(e1, e2, e3) =>
    switch (
      ctx |> checkType(e1, TBool),
      ctx |> checkType(e2, t),
      ctx |> checkType(e3, t),
    ) {
    | (Some(_), Some(_), Some(_)) => Some(t)
    | _ => None
    }
  | _ => ctx |> inferType(e) == Some(t) ? Some(t) : None
  };
