open Expr
open Env.Env

(* This file only contains one module at the moment, but it's intended
   to host smaller modules with self-contained logic helpful to
   the evaluation functions. Can be extended as needed. *)

(* EXPR_CONVERSIONS: functions for converting expressions from one
   type to another.*)
module type EXPR_CONVERSIONS = sig
  val expr_from_val : value -> expr

  val closure_from_val : value -> expr * env

  (* val_from_value: extracts the value from a closure if needed. *)
  val val_from_value : value -> value

  val num_from_expr : expr -> int

  val bool_from_expr : expr -> bool
end

module ExprConversions = struct
  let expr_from_val (value : value) : expr =
    match value with
    | Val expr -> expr
    | Closure _ ->
        err "expr_from_val" "expected Val, received Closure"
          (value_to_string value)

  let closure_from_val (value : value) : expr * env =
    match value with
    | Closure (exp, env) -> (exp, env)
    | Val _ ->
        err "closure_from_val" "expected Closure, received Val"
          (value_to_string value)

  let val_from_value (v : value) : value =
    match v with Val _ -> v | Closure (value, _) -> Val value

  let num_from_expr (exp : expr) : int =
    match exp with
    | Num num -> num
    | _ ->
        err "num_from_expr" "expected Num, received other"
          (Expr.exp_to_abstract_string exp)

  let bool_from_expr (exp : expr) : bool =
    match exp with
    | Bool bl -> bl
    | _ ->
        err "bool_from_expr" "expected Bool, received other"
          (Expr.exp_to_abstract_string exp)
end
