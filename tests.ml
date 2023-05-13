let run_tests () =
  Expr_tests.run_tests ();
  Evaluation_tests.run_tests ()

let () = run_tests ()