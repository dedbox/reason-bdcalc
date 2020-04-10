/******************************************************************************/
/* REPL                                                                       */
/******************************************************************************/

open Bdcalc.Ast;
open Bdcalc.Semantics;
open Bdcalc.Syntax;

open Format;
open Re.Str;

let read = (str, ctx) => {
  switch (parse_expr(str)) {
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
    switch (parse_expr(e_str), parse_type(t_str)) {
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
    | Some(t) => printf("- : %a@.", pp_type, t)
    | None => printf("- Syntax Error!@.")
    };
    repl(ctx);
  };
};

repl(StringMap.empty);
