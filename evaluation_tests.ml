open Evaluation
open Passert
open Env
open Expr
open Eval_utils

let test_eval (suite : string) (eval : Expr.expr -> Env.env -> Env.value) =
  let expect = passert ~suite in
  expect (should_fail (eval Raise) (Env.empty ())) "'Raise' raises error";
  expect (should_fail (eval (Var "x")) (Env.empty ())) "catches unbound var";
  expect (eval (Num 3) (Env.empty ()) = Val (Num 3)) "num | num";
  expect (eval (Bool true) (Env.empty ()) = Val (Bool true)) "bool | bool";
  expect
    (eval (Unop (Negate, Num 3)) (Env.empty ()) = Val (Num (-3)))
    "unop negate";
  expect
    (should_fail (eval (Unop (Negate, Bool true))) (Env.empty ()))
    "negate only applies to nums";
  expect
    (eval (Binop (Plus, Num 2, Num 3)) (Env.empty ()) = Val (Num 5))
    "binop plus";
  expect
    (eval (Binop (Minus, Num 5, Num 7)) (Env.empty ()) = Val (Num (-2)))
    "binop minus";
  expect
    (eval (Binop (Times, Num 2, Num 3)) (Env.empty ()) = Val (Num 6))
    "binop times";
  expect
    (eval (Binop (Equals, Num 2, Num (-2))) (Env.empty ()) = Val (Bool false))
    "binop num equal";
  expect
    (eval (Binop (Equals, Bool false, Bool false)) (Env.empty ())
    = Val (Bool true))
    "binop bool equal";
  expect
    (eval (Binop (LessThan, Num 5, Num 2)) (Env.empty ()) = Val (Bool false))
    "binop less than";
  expect
    (should_fail (eval (Binop (Times, Bool true, Num 3))) (Env.empty ()))
    "binop arithmetic args must be same type";
  expect
    (eval
       (Conditional (Binop (Equals, Bool true, Bool false), Num 2, Num 10))
       (Env.empty ())
    = Val (Num 10))
    "cond false";
  expect
    (eval
       (Conditional (Binop (LessThan, Num 4, Num 6), Bool true, Bool false))
       (Env.empty ())
    = Val (Bool true))
    "cond true";
  expect
    (should_fail
       (eval (Conditional (Binop (Equals, Num 4, Bool true), Num 5, Num 10)))
       (Env.empty ()))
    "cond invalid condition";
  expect
    (ExprConversions.val_from_value
       (eval (Fun ("a", Binop (Equals, Var "a", Bool true))) (Env.empty ()))
    = Val (Fun ("a", Binop (Equals, Var "a", Bool true))))
    "fun | fun";
  expect
    (eval (Let ("x", Num 5, Var "x")) (Env.empty ()) = Val (Num 5))
    "let x = 5";
  expect
    (eval
       (Let ("x", Binop (Times, Num 3, Num 4), Binop (Plus, Var "x", Num 1)))
       (Env.empty ())
    = Val (Num 13))
    "let binding in simple arithmetic";
  expect
    (eval
       (Let ("x", Num 1, Let ("x", Num 6, Binop (Times, Var "x", Var "x"))))
       (Env.empty ())
    = Val (Num 36))
    "nested conflicting lets";
  expect
    (ExprConversions.val_from_value
       (eval
          (Let ("x", Fun ("x", Binop (Plus, Var "x", Num 1)), Var "x"))
          (Env.empty ()))
    = Val (Fun ("x", Binop (Plus, Var "x", Num 1))))
    "let fun eval";
  expect
    (eval
       (App
          ( Let ("x", Num 5, Fun ("y", Binop (Plus, Var "x", Var "y"))),
            Unop (Negate, Num 2) ))
       (Env.empty ())
    = Val (Num 3))
    "let-bound app";
  expect
    (eval
       (App
          ( Letrec
              ( "f",
                Fun
                  ( "n",
                    Conditional
                      ( Binop (Equals, Var "n", Num 0),
                        Num 1,
                        Binop
                          ( Times,
                            Var "n",
                            App (Var "f", Binop (Minus, Var "n", Num 1)) ) ) ),
                Var "f" ),
            Num 2 ))
       (Env.empty ())
    = Val (Num 2))
    "app rec #2";
  expect
    (eval
       (Letrec
          ( "f",
            Fun
              ( "x",
                Conditional
                  ( Binop (Equals, Var "x", Num 0),
                    Num 1,
                    Binop
                      ( Times,
                        Var "x",
                        App (Var "f", Binop (Minus, Var "x", Num 1)) ) ) ),
            App (Var "f", Num 4) ))
       (Env.empty ())
    = Val (Num 24))
    "app rec #2";
  expect
    (eval
       (Let
          ( "x",
            Num 5,
            Let
              ( "y",
                Num 2,
                Letrec
                  ( "z",
                    Fun
                      ( "x",
                        Conditional
                          ( Binop (LessThan, Var "x", Unop (Negate, Num 4)),
                            Var "x",
                            App (Var "z", Binop (Minus, Var "x", Num 8)) ) ),
                    App (Var "z", Var "y") ) ) ))
       (Env.empty ())
    = Val (Num ~-6))
    "app rec #3";
  expect (eval (String "x") (Env.empty ()) = Val (String "x")) "String | String";
  expect
    (should_fail (eval (Binop (Plus, String "hello", Num 5))) (Env.empty ()))
    "prevented from binop across types";
  expect
    (eval
       (Let
          ( "first",
            String "First",
            Let
              ( "second",
                String "Second",
                Conditional
                  ( Binop (Equals, Var "first", Var "second"),
                    String "eq",
                    String "neq" ) ) ))
       (Env.empty ())
    = Val (String "neq"))
    "compare string equality";
  expect
    (eval
       (Let
          ( "z",
            String "not looped",
            Letrec
              ( "x",
                Fun
                  ( "y",
                    Conditional
                      ( Binop (Equals, Var "y", String "looped"),
                        Bool true,
                        App (Var "x", String "looped") ) ),
                App (Var "x", Var "z") ) ))
       (Env.empty ())
    = Val (Bool true))
    "letrec w/ string equality"

let test_env () =
  print_endline "\nTesting env";
  let testable_env : Env.env =
    Env.extend
      (Env.extend
         (Env.extend
            (Env.extend (Env.empty ()) "id-1" (ref (Env.Val (Num 5))))
            "id-2"
            (ref (Env.Val (Bool true))))
         "id-3" (ref (Env.Val (Var "x"))))
      "id-4"
      (ref (Env.Closure (Num 3, Env.empty ())))
  in
  let expect = passert ~suite:"env" in
  expect true ("env_to_string: " ^ Env.env_to_string testable_env);
  expect
    (Env.close (Num 3) (Env.empty ()) = Closure (Num 3, Env.empty ()))
    "basic closure";
  expect (Env.lookup testable_env "id-1" = Val (Num 5)) "lookup id-1";
  expect
    (Env.lookup testable_env "id-4" = Closure (Num 3, Env.empty ()))
    "lookup id-4";
  expect
    (should_fail (Env.lookup testable_env) "id-5")
    "lookup not-present fails";
  expect true
    ("value_to_string printenvp false: "
    ^ Env.value_to_string ~printenvp:false (Closure (Var "x", testable_env)));
  expect true
    ("value_to_string printenvp true: "
    ^ Env.value_to_string (Closure (Var "x", Env.empty ())))

let test_eval_s () =
  print_endline "\nTesting eval_s";
  let suite = "eval_s" in
  let expect = passert ~suite in
  test_eval suite eval_s;
  expect
    (eval_s
       (Let
          ( "x",
            Num 1,
            Let
              ( "f",
                Fun ("y", Binop (Plus, Var "x", Var "y")),
                Let ("x", Num 2, App (Var "f", Num 3)) ) ))
       (Env.empty ())
    = Val (Num 4))
    "(1) sub-specific = 4"

let test_eval_d () =
  print_endline "\nTesting eval_d";
  let suite = "eval_d" in
  let expect = passert ~suite in
  test_eval suite eval_d;
  expect
    (eval_d
       (Let
          ( "x",
            Num 1,
            Let
              ( "f",
                Fun ("y", Binop (Plus, Var "x", Var "y")),
                Let ("x", Num 2, App (Var "f", Num 3)) ) ))
       (Env.empty ())
    = Val (Num 5))
    "(1) dyn-specific = 5"

let test_eval_l () =
  print_endline "\nTesting eval_l";
  let suite = "eval_l" in
  let expect = passert ~suite in
  test_eval suite eval_l;
  expect
    (eval_l
       (Let
          ( "x",
            Num 1,
            Let
              ( "f",
                Fun ("y", Binop (Plus, Var "x", Var "y")),
                Let ("x", Num 2, App (Var "f", Num 3)) ) ))
       (Env.empty ())
    = Val (Num 4))
    "(1) sub/lex-specific = 4"

let run_tests () =
  test_env ();
  test_eval_s ();
  test_eval_d ();
  test_eval_l ()

let () = run_tests ()
