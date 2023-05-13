open Env.Env
open Expr
open Eval_utils.ExprConversions

(* EVAL_INPUTS: modules that are used for configuring the generic evaluator
   with model-specific input functions.

   - Every method is capable of raising an EvalError exception.
   - Methods may have side-effects including reassigning address
     values in the provided environment.
   - Some modules call eachother. While doing so avoids copy-paste code
     among functions that are identical, keep an eye on those methods for
     possible bugs:
       • DynEval.eval_fun returns SubEval.eval_fun
       • LexEval.eval_var returns DynEval.eval_var
       • LexEval.eval_let returns DynEval.eval_let
*)
module type EVAL_INPUTS = sig
  (* eval_var: evaluates a variable in its environment (if applicable). *)
  val eval_var : expr -> env -> value

  (* eval_fun: evaluates a function, performing substitution, renaming, and
     returning a Val or Closure depending on evaluation model. *)
  val eval_fun : expr -> env -> value

  (* eval_let: evaluates a Let binding, performing substitution and environment
     modifications as needed. *)
  val eval_let : expr -> env -> evaluator -> value

  (* eval_letrec: evaluates a Letrec binding, handling substitution, environment
     modifications, and recursive-specific cases as needed. *)
  val eval_letrec : expr -> env -> evaluator -> value

  (* eval_app: applies one expression to another, handling substitution and
     environment modifications to reach an atomic value. *)
  val eval_app : expr -> env -> evaluator -> value

  (* config: packages the above functions into a record that can be easily
     passed to the generic evaluator. *)
  val config : Eval_common.eval_inputs
end

(* ............................................................

     SUBEVAL: Evaluation functions specific to Substitution semantics.

   ............................................................... *)

module SubEval : EVAL_INPUTS = struct
  let eval_var (exp : expr) (_ : env) : value =
    let context = Expr.exp_to_abstract_string exp in
    match exp with
    | Var _ -> err "sub_eval_var" "unbound variable" context
    | _ -> err "sub_eval_var" "expected Var, received other" context

  let eval_fun (exp : expr) (_ : env) : value =
    match exp with
    | Fun _ -> Val exp
    | _ ->
        err "sub_eval_fun" "expected Fun, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_let (exp : expr) (env : env) (eval_s : evaluator) : value =
    match exp with
    | Let (varname, value, _in) ->
        let bound_value = expr_from_val (eval_s value env) in
        eval_s (subst varname bound_value _in) env
    | _ ->
        err "sub_eval_let" "expected Let, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_letrec (exp : expr) (env : env) (eval_s : evaluator) : value =
    match exp with
    | Letrec (varname, value, _in) ->
        let bound_value = expr_from_val (eval_s value env) in
        (* rec_sub: the expression with instances of the recursive function's
           name replaced with the actual body of the recursive function. *)
        let rec_sub =
          subst varname (Letrec (varname, bound_value, Var varname)) bound_value
        in
        eval_s (subst varname rec_sub _in) env
    | _ ->
        err "sub_eval_letrec" "expected Letrec, received other"
          (Expr.exp_to_abstract_string exp)

  (* substitution_application: a helper function that applies one function to another
     using semantic evaluation rules.

     If it receives a function, evalues the body with the varname variable bound to the
     value of exp2.
     If it receives a Let or Letrec, passes the application down a layer to the value
     encompassed by the Let or Letrec statement performing necessary substitutions in
     the process. *)
  let substitution_application (exp1 : expr) (exp2 : expr) (env : env)
      (eval_s : evaluator) : value =
    match exp1 with
    | Fun (varname, exp) ->
        eval_s (subst varname (expr_from_val (eval_s exp2 env)) exp) env
    | Let _ | Letrec _ ->
        (* evaluate the let-bound value, perform substitution, and apply to
           the value's child until a function definition is reached. *)
        eval_s (App (expr_from_val (eval_s exp1 env), exp2)) env
    | _ ->
        err "substitution_application" "cannot apply value to non-function"
          (Expr.exp_to_abstract_string (App (exp1, exp2)))

  let eval_app (exp : expr) (env : env) (eval_s : evaluator) : value =
    match exp with
    | App (exp1, exp2) -> substitution_application exp1 exp2 env eval_s
    | _ ->
        err "sub_eval_app" "expected App, received other"
          (Expr.exp_to_abstract_string exp)

  let config : Eval_common.eval_inputs =
    {
      ev_var = eval_var;
      ev_fun = eval_fun;
      ev_let = eval_let;
      ev_letrec = eval_letrec;
      ev_app = eval_app;
    }
end

(* ............................................................

     DYNEVAL: Evaluation functions specific to Dynamic semantics.

   ............................................................... *)

module DynEval : EVAL_INPUTS = struct
  let eval_var (exp : expr) (env : env) : value =
    match exp with
    | Var varname -> lookup env varname
    | _ ->
        err "dyn_eval_var" "expected Var, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_fun = SubEval.eval_fun

  let eval_let (exp : expr) (env : env) (eval_d : evaluator) : value =
    match exp with
    | Let (varname, value, _in) | Letrec (varname, value, _in) ->
        eval_d _in (extend env varname (ref (eval_d value env)))
    | _ ->
        err "dyn_eval_let" "expected Let, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_letrec = eval_let

  (* dynamic_application: helper function that applies one expression
     to another using dynamic evaluation rules.

     exp1 must be evaluated in the context of its bound values.
     Because bound variables are tracked in an environment that
     can't be passed from evaluation to another, we need to either
     apply a value to a function if exp1 is a function or update
     the environment with a new let (rec) expression any pass the
     application to the child expression with an updated environment.
     If a value cannot be applied, throws an error. *)
  let dynamic_application (exp1 : expr) (exp2 : expr) (env : env)
      (eval_d : evaluator) : value =
    let nm = new_varname () in
    (* function varnames are substituted to be reserved strings that will
       never conflict. *)
    match exp1 with
    | Fun (varname, exp) ->
        eval_d
          (subst varname (Var nm) exp)
          (extend env nm (ref (eval_d exp2 env)))
    | Let (varname, value, exp) | Letrec (varname, value, exp) ->
        eval_d (App (exp, exp2)) (extend env varname (ref (eval_d value env)))
    | Var _ -> eval_d (App (expr_from_val (eval_d exp1 env), exp2)) env
    | _ ->
        err "dynamic_application" "cannot apply value to non-function"
          (Expr.exp_to_abstract_string (App (exp1, exp2)))

  let eval_app (exp : expr) (env : env) (eval_d : evaluator) : value =
    match exp with
    | App (exp1, exp2) -> dynamic_application exp1 exp2 env eval_d
    | _ ->
        err "dyn_eval_app" "expected App, received other"
          (Expr.exp_to_abstract_string exp)

  let config : Eval_common.eval_inputs =
    {
      ev_var = eval_var;
      ev_fun = eval_fun;
      ev_let = eval_let;
      ev_letrec = eval_letrec;
      ev_app = eval_app;
    }
end

(* ............................................................

     LEXEVAL: Evaluation functions specific to Lexical semantics.

   ............................................................... *)

module LexEval : EVAL_INPUTS = struct
  let eval_var = DynEval.eval_var

  let eval_fun (exp : expr) (env : env) : value =
    match exp with
    | Fun _ -> close exp env
    | _ ->
        err "lex_eval_fun" "expected Fun, received other"
          (Expr.exp_to_abstract_string exp)

  let eval_let = DynEval.eval_let

  (* eval_letrec: creates a temporary env with an unassigned mapping
     to the recursive binding, evaluates the body of the letrec to a
     fun value and its environment, updates environments with the Closure
     containing the evaluated function and its environment, updates the
     unassigned mapping with the recursive function's value, and evaluates
     the _in value.
     Notice - there are two distinct environments at play. `_in_env` is
     the environment we evaluate the final child expression with. But, there's
     an additional `closed_value_env` that contains the environment for the
     value function as it was when defined. That's why there are two `extend`
     calls at the end updating the Unassigned values in both envs. *)
  let eval_letrec (exp : expr) (env : env) (eval_l : evaluator) : value =
    match exp with
    | Letrec (varname, value, _in) ->
        let temp_env = extend env varname (ref (Val Unassigned)) in
        let value_fun, closed_value_env =
          closure_from_val (eval_l value temp_env)
        in
        let replace_unassigned = ref (Closure (value_fun, closed_value_env)) in
        let _ = extend closed_value_env varname replace_unassigned in
        let _in_env = extend temp_env varname replace_unassigned in
        eval_l _in _in_env
    | _ ->
        err "lex_eval_letrec" "expected Letrec, received other"
          (Expr.exp_to_abstract_string exp)

  (* lexical_application: helper function that applies one expression to
     another using lexical evaluation rules.

     Evaluates exp1 to a function and its closure. Extends the closure with
     the variable bound by the function and it's applied value. Evaluates
     the resulting term. *)
  let lexical_application (exp1 : expr) (exp2 : expr) (_env : env)
      (eval_l : evaluator) : value =
    let fun_exp, fun_env = closure_from_val (eval_l exp1 _env) in
    let safe_varname = new_varname () in
    (* replace the current varname with 'safe varname', which is guaranteed to be
       a unique reserved string. *)
    match fun_exp with
    | Fun (varname, body) ->
        eval_l
          (subst varname (Var safe_varname) body)
          (extend fun_env safe_varname (ref (eval_l exp2 _env)))
    | _ ->
        err "lexical_application" "cannot apply value to non-function"
          (Expr.exp_to_abstract_string (App (exp1, exp2)))

  let eval_app (exp : expr) (env : env) (eval_l : evaluator) : value =
    match exp with
    | App (exp1, exp2) -> lexical_application exp1 exp2 env eval_l
    | _ ->
        err "lex_eval_app" "expected App, received other"
          (Expr.exp_to_abstract_string exp)

  let config : Eval_common.eval_inputs =
    {
      ev_var = eval_var;
      ev_fun = eval_fun;
      ev_let = eval_let;
      ev_letrec = eval_letrec;
      ev_app = eval_app;
    }
end
