open Bdcalc.Ast.HExpr;
open Bdcalc.Syntax;

/******************************************************************************/
/* parse_type                                                                 */
/******************************************************************************/

let test_parse_type_failure = () =>
  Alcotest.(check(bool))(
    "parse_type failure",
    true,
    parse_type("123") == None,
  );

/* TBool */

let test_parse_type_bool = () =>
  Alcotest.(check(bool))(
    "parse_type bool",
    true,
    parse_type("Bool") == Some(TBool),
  );

/* TFun */

let test_parse_type_arrow = () =>
  Alcotest.(check(bool))(
    "parse_type arrow",
    true,
    parse_type("Bool -> Bool") == Some(TFun(TBool, TBool)),
  );

let test_parse_type_arrows = () =>
  Alcotest.(check(bool))(
    "parse_type arrows",
    true,
    parse_type("Bool -> Bool -> Bool")
    == Some(TFun(TBool, TFun(TBool, TBool))),
  );

let test_parse_type_arrows_left = () =>
  Alcotest.(check(bool))(
    "parse_type arrows left",
    true,
    parse_type("(Bool -> Bool) -> Bool")
    == Some(TFun(TFun(TBool, TBool), TBool)),
  );

let test_parse_type_arrows_right = () =>
  Alcotest.(check(bool))(
    "parse_type arrows right",
    true,
    parse_type("Bool -> (Bool -> Bool)")
    == Some(TFun(TBool, TFun(TBool, TBool))),
  );

/******************************************************************************/
/* parse_expr                                                                 */
/******************************************************************************/

let test_parse_expr_failure = () =>
  Alcotest.(check(bool))(
    "parse_expr failure",
    true,
    parse_expr("123") == None,
  );

/* Var */

let test_parse_expr_var = () =>
  Alcotest.(check(bool))(
    "parse_expr var",
    true,
    parse_expr("x") == Some(Var("x")),
  );

/* App */

let test_parse_expr_app = () =>
  Alcotest.(check(bool))(
    "parse_expr app",
    true,
    parse_expr("f x") == Some(App(Var("f"), Var("x"))),
  );

let test_parse_expr_apps = () =>
  Alcotest.(check(bool))(
    "parse_expr apps",
    true,
    parse_expr("f x y") == Some(App(App(Var("f"), Var("x")), Var("y"))),
  );

let test_parse_expr_apps_left = () =>
  Alcotest.(check(bool))(
    "parse_expr apps left",
    true,
    parse_expr("(f x) y")
    == Some(App(App(Var("f"), Var("x")), Var("y"))),
  );

let test_parse_expr_apps_right = () =>
  Alcotest.(check(bool))(
    "parse_expr apps right",
    true,
    parse_expr("f (x y)")
    == Some(App(Var("f"), App(Var("x"), Var("y")))),
  );

/* Fun */

let test_parse_expr_fun = () =>
  Alcotest.(check(bool))(
    "parse_expr fun",
    true,
    parse_expr("\\x.x") == Some(Fun("x", Var("x"))),
  );

let test_parse_expr_fun_app = () =>
  Alcotest.(check(bool))(
    "parse_expr fun app",
    true,
    parse_expr("\\x.x x") == Some(Fun("x", App(Var("x"), Var("x")))),
  );

let test_parse_expr_funs = () =>
  Alcotest.(check(bool))(
    "parse_expr funs",
    true,
    parse_expr("\\x.\\y.x") == Some(Fun("x", Fun("y", Var("x")))),
  );

let test_parse_expr_funs_app = () =>
  Alcotest.(check(bool))(
    "parse_expr funs app",
    true,
    parse_expr("\\f.\\x.f x")
    == Some(Fun("f", Fun("x", App(Var("f"), Var("x"))))),
  );

let test_parse_expr_true = () =>
  Alcotest.(check(bool))(
    "parse_expr true",
    true,
    parse_expr("true") == Some(Tru),
  );

let test_parse_expr_false = () =>
  Alcotest.(check(bool))(
    "parse_expr false",
    true,
    parse_expr("false") == Some(Fls),
  );

let test_parse_expr_if = () =>
  Alcotest.(check(bool))(
    "parse_expr if",
    true,
    parse_expr("if true false true") == Some(If(Tru, Fls, Tru)),
  );

let test_parse_expr_ann_var = () =>
  Alcotest.(check(bool))(
    "parse_expr annotate var",
    true,
    parse_expr("x : Bool") == Some(Ann(Var("x"), TBool)),
  );

let test_parse_expr_ann_fun = () =>
  Alcotest.(check(bool))(
    "parse_expr annotate fun",
    true,
    parse_expr("(\\x.x) : Bool -> Bool")
    == Some(Ann(Fun("x", Var("x")), TFun(TBool, TBool))),
  );

let test_parse_expr_ann_in_fun = () =>
  Alcotest.(check(bool))(
    "parse_expr annotate in fun",
    true,
    parse_expr("\\f.f : Bool -> Bool")
    == Some(Fun("f", Ann(Var("f"), TFun(TBool, TBool)))),
  );

let () =
  Alcotest.(
    run(
      "Syntax",
      [
        (
          "parse_type",
          [
            test_case("failure", `Quick, test_parse_type_failure),
            test_case("bool", `Quick, test_parse_type_bool),
            test_case("arrow", `Quick, test_parse_type_arrow),
            test_case("arrows", `Quick, test_parse_type_arrows),
            test_case("arrows left", `Quick, test_parse_type_arrows_left),
            test_case("arrows right", `Quick, test_parse_type_arrows_right),
          ],
        ),
        (
          "parse_expr",
          [
            test_case("failure", `Quick, test_parse_expr_failure),
            test_case("var", `Quick, test_parse_expr_var),
            test_case("app", `Quick, test_parse_expr_app),
            test_case("apps", `Quick, test_parse_expr_apps),
            test_case("apps left", `Quick, test_parse_expr_apps_left),
            test_case("apps right", `Quick, test_parse_expr_apps_right),
            test_case("fun", `Quick, test_parse_expr_fun),
            test_case("fun app", `Quick, test_parse_expr_fun_app),
            test_case("funs", `Quick, test_parse_expr_funs),
            test_case("funs app", `Quick, test_parse_expr_funs_app),
            test_case("true", `Quick, test_parse_expr_true),
            test_case("false", `Quick, test_parse_expr_false),
            test_case("if", `Quick, test_parse_expr_if),
            test_case("annotate var", `Quick, test_parse_expr_ann_var),
            test_case("annotate fun", `Quick, test_parse_expr_ann_fun),
            test_case("annotate in fun", `Quick, test_parse_expr_ann_in_fun),
          ],
        ),
      ],
    )
  );
