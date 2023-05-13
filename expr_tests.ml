open Expr
open Passert

let test_exp_to_concrete_string () =
  print_endline "\nTesting exp_to_concrete_string";
  let expect = passert ~suite:"exp_to_concrete_string" in
  expect true (exp_to_concrete_string (Var "testvar"));
  expect true (exp_to_concrete_string (Num 5));
  expect true (exp_to_concrete_string (Bool true));
  expect true (exp_to_concrete_string (Unop (Negate, Num 5)));
  expect true (exp_to_concrete_string (Binop (LessThan, Num 3, Num 7)));
  expect true
    (exp_to_concrete_string
       (Conditional (Binop (Equals, Bool true, Bool true), Num 1, Num 0)));
  expect true
    (exp_to_concrete_string (Let ("x", Num 5, Binop (Plus, Var "x", Num 2))));
  expect true
    (exp_to_concrete_string (Letrec ("x", Num 5, Binop (Plus, Var "x", Num 2))));
  expect true
    (exp_to_concrete_string
       (Fun ("x", Let ("y", Var "x", Binop (Plus, Var "y", Num 2)))));
  expect true (exp_to_concrete_string Raise);
  expect true (exp_to_concrete_string Unassigned);
  expect true
    (exp_to_concrete_string
       (App (Fun ("x", Binop (Plus, Var "x", Num 2)), Num 3)))

let test_exp_to_abstract_string () =
  print_endline "\nTesting exp_to_abstract_string";
  let expect = passert ~suite:"exp_to_abstract_string" in
  expect true (exp_to_abstract_string (Var "testvar"));
  expect true (exp_to_abstract_string (Num 5));
  expect true (exp_to_abstract_string (Bool true));
  expect true (exp_to_abstract_string (Unop (Negate, Num 5)));
  expect true (exp_to_abstract_string (Binop (LessThan, Num 3, Num 7)));
  expect true
    (exp_to_abstract_string
       (Conditional (Binop (Equals, Bool true, Bool true), Num 1, Num 0)));
  expect true
    (exp_to_abstract_string
       (Fun ("x", Let ("y", Var "x", Binop (Plus, Var "y", Num 2)))));
  expect true
    (exp_to_abstract_string (Let ("x", Num 5, Binop (Plus, Var "x", Num 2))));
  expect true
    (exp_to_abstract_string (Letrec ("x", Num 5, Binop (Plus, Var "x", Num 2))));
  expect true (exp_to_abstract_string Raise);
  expect true (exp_to_abstract_string Unassigned);
  expect true
    (exp_to_abstract_string
       (App (Fun ("x", Binop (Plus, Var "x", Num 2)), Num 3)))

let test_free_vars () =
  print_endline "\nTesting free_vars";
  let expect = passert ~suite:"free_vars" in
  expect
    (list_of_vars (free_vars (Binop (Plus, Num 5, Var "x"))) = [ "x" ])
    "simple binop";
  expect
    (list_of_vars
       (free_vars
          (Let
             ( "x",
               Num 3,
               Let ("y", Var "x", App (App (Var "f", Var "x"), Var "y")) )))
    = [ "f" ])
    "complex single free";
  expect
    (list_of_vars
       (free_vars
          (Let
             ( "x",
               Var "x",
               Let ("y", Var "x", App (App (Var "f", Var "x"), Var "y")) )))
    = [ "f"; "x" ])
    "complex multi free";
  expect
    (list_of_vars
       (free_vars
          (Let
             ( "x",
               Var "y",
               Let ("y", Var "x", App (App (Var "f", Var "x"), Var "y")) )))
    = [ "f"; "y" ])
    "variation of previous";
  expect
    (list_of_vars (free_vars (Let ("x", Fun ("y", Var "x"), Var "x"))) = [ "x" ])
    "tricky scoping";
  expect
    (list_of_vars
       (free_vars
          (Letrec
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
               App (Var "f", Num 2) )))
    = [])
    "recursive eval";
  expect
    (list_of_vars
       (free_vars
          (Let
             ( "x",
               Binop (Plus, Num 3, Num 5),
               App
                 ( Fun ("x", Binop (Times, Var "x", Var "x")),
                   Binop (Minus, Var "x", Num 2) ) )))
    = [])
    "fun and app eval";
  expect
    (list_of_vars
       (free_vars
          (Let
             ( "y",
               Binop (Plus, Var "y", Num 2),
               Let
                 ( "g",
                   Fun ("x", Binop (Plus, Var "y", Num 5)),
                   App (Fun ("z", App (Var "z", Var "d")), Var "g") ) )))
    = [ "d"; "y" ])
    "scoping, fun, and app"

module StrSet = Set.Make (String)

let test_new_varname () =
  print_endline "\nTesting new_varname";
  let expect = passert ~suite:"new_varname" in
  let rec add_varnames (ss : StrSet.t) (count : int) : StrSet.t =
    if count < 1 then ss
    else add_varnames (StrSet.add (new_varname ()) ss) (count - 1)
  in
  expect
    (StrSet.cardinal (add_varnames StrSet.empty 5) = 5)
    "created five distinct varids"

let test_subst () =
  print_endline "\nTesting subst";
  let injected_new_varname () : string = "var-0" in
  let expect = passert ~suite:"subst" in
  expect
    (subst "x" (Num 3) (Binop (Plus, Var "x", Var "y"))
    = Binop (Plus, Num 3, Var "y"))
    "x = 3";
  expect
    (subst "y" (Num 3) (Binop (Plus, Var "x", Var "y"))
    = Binop (Plus, Var "x", Num 3))
    "y = 3";
  expect
    (subst "z" (Num 3) (Binop (Plus, Var "x", Var "y"))
    = Binop (Plus, Var "x", Var "y"))
    "no substitution";
  expect
    (subst "x" (Num 7) (Let ("x", Num 3, Let ("x", Num 5, Var "x")))
    = Let ("x", Num 3, Let ("x", Num 5, Var "x")))
    "no substitution w/ scoping";
  expect
    (subst "x" (Num 8)
       (App
          ( Fun ("x", Binop (Times, Var "x", Var "x")),
            Binop (Minus, Var "x", Num 2) ))
    = App
        (Fun ("x", Binop (Times, Var "x", Var "x")), Binop (Minus, Num 8, Num 2))
    )
    "fun and app";
  expect
    (subst "y" (Num 3)
       (Let
          ("x", Binop (Times, Var "y", Var "y"), Binop (Plus, Var "x", Var "x")))
    = Let ("x", Binop (Times, Num 3, Num 3), Binop (Plus, Var "x", Var "x")))
    "let binding";
  expect
    (subst ~new_varname:injected_new_varname "x"
       (Binop (Plus, Var "y", Num 1))
       (Fun ("y", Binop (Plus, Binop (Plus, Var "x", Var "y"), Num 3)))
    = Fun
        ( "var-0",
          Binop
            ( Plus,
              Binop (Plus, Binop (Plus, Var "y", Num 1), Var "var-0"),
              Num 3 ) ))
    "variable substitution";
  expect
    (subst "x" (Num 3) (Letrec ("x", Fun ("y", Var "x"), Var "x"))
    = Letrec ("x", Fun ("y", Var "x"), Var "x"))
    "let rec bound"

let run_tests () =
  test_exp_to_concrete_string ();
  test_exp_to_abstract_string ();
  test_free_vars ();
  test_new_varname ();
  test_subst ()

let () = run_tests ()
