open HType;
open HExpr;

/******************************************************************************/
/* Typing Contexts                                                            */
/******************************************************************************/

module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

let lookup = (_, _) => None;

/******************************************************************************/
/* Bidirectional Type Checking                                                */
/******************************************************************************/

let rec inferType = (ctx, e) =>
  switch (e) {
  | Var(x) => lookup(ctx, x)
  | Tru
  | Fls => Some(HType.TBool)
  | Ann(e, t) => checkType(ctx, e, t)
  | App(e1, e2) =>
    switch (inferType(ctx, e1)) {
    | Some(TFun(t1, t2)) =>
      switch (checkType(ctx, e2, t1)) {
      | Some(_) => Some(t2)
      | None => None
      }
    | _ => None
    }
  | _ => None
  }

and checkType = (ctx, e, t) =>
  switch (e) {
  | If(e1, e2, e3) =>
    switch (
      checkType(ctx, e1, TBool),
      checkType(ctx, e2, t),
      checkType(ctx, e3, t),
    ) {
    | (Some(_), Some(_), Some(_)) => Some(t)
    | _ => None
    }
  | _ => None
  };

/******************************************************************************/
/* Runtime                                                                    */
/******************************************************************************/

let g: StringMap.t(HType.t) = StringMap.empty;

/******************************************************************************/
/* REPL                                                                       */
/******************************************************************************/

Format.printf("%a@.", HExpr.pp, App(App(Var("x"), Var("y")), Var("z")));
Format.printf("%a@.", HExpr.pp, App(Var("x"), App(Var("y"), Var("z"))));
Format.printf(
  "%a@.",
  HExpr.pp,
  App(
    App(
      App(App(Var("x"), Var("y")), App(Var("z"), Var("w"))),
      Var("v"),
    ),
    Var("u"),
  ),
);

let parse = s => {
  let lexbuf = Lexing.from_string(s);
  Parser.main(Lexer.read, lexbuf);
};

Format.printf("%a@.", HExpr.pp, parse("\\f.\\x.\\y.f x y z"));
Format.printf("%a@.", HExpr.pp, parse("\\f.\\x.\\y.f x (y z)"));

Format.printf("%a@.", HExpr.pp, parse("true : Bool"));
Format.printf("%a@.", HExpr.pp, parse("(\\x.x) : Bool -> Bool"));
Format.printf("%a@.", HExpr.pp, parse("(\\x.x) a : Bool"));
Format.printf("%a@.", HExpr.pp, parse("((\\x.x) a) : Bool"));
Format.printf(
  "%a@.",
  HExpr.pp,
  parse("(\\x.\\y.x y) : Bool -> Bool -> Bool"),
);
Format.printf(
  "%a@.",
  HExpr.pp,
  parse("(\\x.x true) : (Bool -> Bool) -> Bool"),
);
