(*
                          CS 51 Final Project
                          MiniML -- Evaluation
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
*)

open Eval_common
open Eval_distinct
open Expr
open Env

(* The trivial evaluator *)
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp

(* The substitution model evaluator *)
let eval_s (_exp : expr) (_env : Env.env) : Env.value =
  EvalCommon.generic_eval SubEval.config _exp _env

(* The dynamically-scoped environment model evaluator *)
let eval_d (_exp : expr) (_env : Env.env) : Env.value =
  EvalCommon.generic_eval DynEval.config _exp _env

(* The lexically-scoped environment model evaluator *)
let eval_l (_exp : expr) (_env : Env.env) : Env.value =
  EvalCommon.generic_eval LexEval.config _exp _env

(* Determines the evaluator connected to MiniML *)
let evaluate = eval_l
