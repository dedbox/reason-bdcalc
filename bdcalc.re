open HType;
open HExpr;

open Format;
open Re.Str;

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
  | Fls => Some(HType.TBool)
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

/******************************************************************************/
/* REPL                                                                       */
/******************************************************************************/

let parse = s => {
  let lexbuf = Lexing.from_string(s);
  try(Some(Parser.top(Lexer.read, lexbuf))) {
  | Lexer.Error(_)
  | Parser.Error => None
  };
};

let parse_type = s => {
  let lexbuf = Lexing.from_string(s);
  try(Some(Parser.typ_top(Lexer.read, lexbuf))) {
  | Lexer.Error(_)
  | Parser.Error => None
  };
};

let read = (str, ctx) => {
  switch (parse(str)) {
  | Some(e) => ctx |> inferType(e)
  | None => None
  };
};

type command =
  | Let(string, HType.t)
  | Error
  | Pass;

let top_parse = (s, ctx) => {
  let rx = regexp("^let +\\([a-z]+\\) +: +\\([^=]+\\) += +\\(.+\\)");
  if (string_match(rx, s, 0)) {
    let (x, t_str, e_str) = (
      matched_group(1, s),
      matched_group(2, s),
      matched_group(3, s),
    );
    switch (parse(e_str), parse_type(t_str)) {
    | (Some(e), Some(t)) =>
      switch (ctx |> checkType(e, t)) {
      | Some(_) => Let(x, t)
      | None => Error
      }
    | _ => Error
    };
  } else {
    Pass;
  };
};

let rec repl = ctx => {
  printf("> @?");
  let str = read_line();
  switch (ctx |> top_parse(str)) {
  | Let(x, t) => repl(ctx |> extend(x, t))
  | Error =>
    printf("- Invalid Command!@.");
    repl(ctx);
  | Pass =>
    switch (ctx |> read(str)) {
    | Some(t) => printf("- : %a@.", HType.pp, t)
    | None => printf("- Syntax Error!@.")
    };
    repl(ctx);
  };
};

repl(StringMap.empty);

/******************************************************************************/
/* Examples                                                                   */
/******************************************************************************/

/* printf("%a@.", HExpr.pp, App(App(Var("x"), Var("y")), Var("z"))); */
/* printf("%a@.", HExpr.pp, App(Var("x"), App(Var("y"), Var("z")))); */
/* printf( */
/*   "%a@.", */
/*   HExpr.pp, */
/*   App( */
/*   App( */
/*     App(App(Var("x"), Var("y")), App(Var("z"), Var("w"))), */
/*     Var("v"), */
/*   ), */
/*   Var("u"), */
/* ), */
/* ); */

/* printf("%a@.", HExpr.pp, parse("\\f.\\x.\\y.f x y z")); */
/* printf("%a@.", HExpr.pp, parse("\\f.\\x.\\y.f x (y z)")); */
/* printf("%a@.", HExpr.pp, parse("true : Bool")); */
/* printf("%a@.", HExpr.pp, parse("(\\x.x) : Bool -> Bool")); */
/* printf("%a@.", HExpr.pp, parse("(\\x.x) a : Bool")); */
/* printf("%a@.", HExpr.pp, parse("((\\x.x) a) : Bool")); */
/* printf( */
/*   "%a@.", */
/*   HExpr.pp, */
/*   parse("(\\x.\\y.x y) : Bool -> Bool -> Bool"), */
/* ); */
/* printf( */
/*   "%a@.", */
/*   HExpr.pp, */
/*   parse("(\\x.x true) : (Bool -> Bool) -> Bool"), */
/* ); */
