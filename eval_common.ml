open Expr
open Env.Env
open Eval_utils.ExprConversions

(* eval_inputs: specifies the signature required by generic_eval *)
type eval_inputs = {
  ev_var : expr -> env -> value;
  ev_fun : expr -> env -> value;
  ev_let : expr -> env -> evaluator -> value;
  ev_letrec : expr -> env -> evaluator -> value;
  ev_app : expr -> env -> evaluator -> value;
}

(* EVAL_COMMON: module containing evaluation logic identical for all of
   the semantic evaluators. `generic_eval` is by-far the most important.
   The other methods are exposed for testing purposes. *)
module type EVAL_COMMON = sig
  (* generic_eval: given an input object of model-specific evaluation functions,
     returns an evaluator capable of evaluating MiniML expressions into atomic values. *)
  val generic_eval : eval_inputs -> evaluator

  (* all below: evaluates expressions of their named type. All throw an error if they
     receive an expression of a type other than their own.
     Note - while functions are also atomic data types in MiniML, they're distinguished
     from the rest because they have special evaluation rules. *)
  val eval_atomic : expr -> value

  val eval_unop : expr -> env -> evaluator -> value

  val eval_binop : expr -> env -> evaluator -> value

  val eval_conditional : expr -> env -> evaluator -> value

  val eval_raise : unit -> value

  val eval_unassigned : expr -> value
end

module EvalCommon : EVAL_COMMON = struct
  let expr_equal (exp1 : expr) (exp2 : expr) : bool =
    match (exp1, exp2) with
    | Num num1, Num num2 -> num1 = num2
    | Bool bl1, Bool bl2 -> bl1 = bl2
    | String str1, String str2 -> str1 = str2
    | _ ->
        err "expr_equal"
          "binop compare only compares int-int, bool-bool, and string-string"
          (Expr.exp_to_abstract_string exp1
          ^ ", "
          ^ Expr.exp_to_abstract_string exp2)

  let apply_unop (op : unop) (exp : expr) : expr =
    match op with Negate -> Num (-1 * num_from_expr exp)

  let apply_binop (op : binop) (exp1 : expr) (exp2 : expr) : expr =
    match op with
    | Plus -> Num (num_from_expr exp1 + num_from_expr exp2)
    | Minus -> Num (num_from_expr exp1 - num_from_expr exp2)
    | Times -> Num (num_from_expr exp1 * num_from_expr exp2)
    | Equals -> Bool (expr_equal exp1 exp2)
    | LessThan -> Bool (num_from_expr exp1 < num_from_expr exp2)

  let eval_atomic (exp : expr) : value =
    match exp with
    | Num _ | Bool _ | String _ -> Val exp
    | _ ->
        err "eval_atomic" "expected Num or Bool, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_unop (exp : expr) (env : env) (eval : evaluator) : value =
    match exp with
    | Unop (op, exp) -> Val (apply_unop op (expr_from_val (eval exp env)))
    | _ ->
        err "eval_unop" "expected Unop, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_binop (exp : expr) (env : env) (eval : evaluator) : value =
    match exp with
    | Binop (op, exp1, exp2) ->
        Val
          (apply_binop op
             (expr_from_val (eval exp1 env))
             (expr_from_val (eval exp2 env)))
    | _ ->
        err "eval_binop" "expected Binop, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_conditional (exp : expr) (env : env) (eval : evaluator) : value =
    match exp with
    | Conditional (_if, _then, _else) ->
        if bool_from_expr (expr_from_val (eval _if env)) then eval _then env
        else eval _else env
    | _ ->
        err "eval_conditional" "expected Conditional, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_raise () : value = raise EvalException

  let eval_unassigned (exp : expr) : value =
    err "eval" "Unassigned cannot be evaluated"
      (Expr.exp_to_abstract_string exp)

  let rec generic_eval
      ({ ev_var; ev_fun; ev_let; ev_letrec; ev_app } as config : eval_inputs)
      (exp : expr) (env : env) : value =
    let applied_eval : evaluator = generic_eval config in
    match exp with
    (* evaluations that are the same regardless of paradigm *)
    | Num _ -> eval_atomic exp
    | Bool _ -> eval_atomic exp
    | String _ -> eval_atomic exp
    | Unop _ -> eval_unop exp env applied_eval
    | Binop _ -> eval_binop exp env applied_eval
    | Conditional _ -> eval_conditional exp env applied_eval
    | Raise -> eval_raise ()
    | Unassigned -> eval_unassigned exp
    (* evaluations that vary by semantic paradigm *)
    | Var _ -> ev_var exp env
    | Fun _ -> ev_fun exp env
    | Let _ -> ev_let exp env applied_eval
    | Letrec _ -> ev_letrec exp env applied_eval
    | App _ -> ev_app exp env applied_eval
end
