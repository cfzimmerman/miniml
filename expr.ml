(* Expr: Defines the abstract syntax of MiniML expressions: *)

type unop = Negate

type binop = Plus | Minus | Times | Equals | LessThan

type varid = string

type expr =
  | Var of varid (* variables *)
  | Num of int (* integers *)
  | Bool of bool (* booleans *)
  | String of string (* strings *)
  | Unop of unop * expr (* unary operators *)
  | Binop of binop * expr * expr (* binary operators *)
  | Conditional of expr * expr * expr (* if then else *)
  | Fun of varid * expr (* function definitions *)
  | Let of varid * expr * expr (* local naming *)
  | Letrec of varid * expr * expr (* recursive local naming *)
  | Raise (* exceptions *)
  | Unassigned (* (temporarily) unassigned *)
  | App of expr * expr
(* function applications *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
  type t = varid

  let compare = String.compare
end)

type varidset = SS.t

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool = SS.equal

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset = SS.of_list

(* list_of_vars: creates a list from a set of variable IDs.
   Useful for testing. *)
let list_of_vars (vars : varidset) : varid list = List.of_seq (SS.to_seq vars)

(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (expression : expr) : varidset =
  match expression with
  | Var name -> SS.singleton name
  | Num _ | Bool _ | String _ | Raise | Unassigned -> SS.empty
  | Unop (_, exp) -> free_vars exp
  | Binop (_, exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)
  | Conditional (_if, _then, _else) ->
      SS.union (SS.union (free_vars _if) (free_vars _else)) (free_vars _then)
  | Fun (name, exp) -> SS.remove name (free_vars exp)
  | Let (name, exp1, exp2) ->
      SS.union (SS.remove name (free_vars exp2)) (free_vars exp1)
  | Letrec (name, exp1, exp2) ->
      SS.union
        (SS.remove name (free_vars exp2))
        (SS.remove name (free_vars exp1))
  | App (exp1, exp2) -> SS.union (free_vars exp1) (free_vars exp2)

(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no other variable names
   use the prefix "var". (Otherwise, they might accidentally be the
   same as a generated variable name.) *)
let new_varname =
  let counter = ref 0 in
  fun () : varid ->
    let temp : int = !counter in
    counter := !counter + 1;
    "var-" ^ string_of_int temp

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst ?(new_varname = new_varname) (var_name : varid) (repl : expr)
    (expression : expr) : expr =
  if SS.mem var_name (free_vars expression) then
    match expression with
    | Var name -> if name = var_name then repl else Var name
    | Unop (op, exp) -> Unop (op, subst var_name repl exp)
    | Binop (op, exp1, exp2) ->
        Binop (op, subst var_name repl exp1, subst var_name repl exp2)
    | Conditional (_if, _else, _then) ->
        Conditional
          ( subst var_name repl _if,
            subst var_name repl _else,
            subst var_name repl _then )
    | Fun (funbound_name, exp) ->
        let funbound_repl = new_varname () in
        Fun
          ( funbound_repl,
            subst var_name repl (subst funbound_name (Var funbound_repl) exp) )
    | Let (name, value, _in) ->
        Let (name, subst var_name repl value, subst var_name repl _in)
    | Letrec (name, value, _in) ->
        Letrec (name, subst var_name repl value, subst var_name repl _in)
    | App (exp1, exp2) ->
        App (subst var_name repl exp1, subst var_name repl exp2)
    | Num _ | Bool _ | String _ | Raise | Unassigned ->
        failwith "logic failure: no free vars"
  else expression

let get_concrete_binop (bn : binop) : string =
  match bn with
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Equals -> "="
  | LessThan -> "<"

(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (expression : expr) : string =
  match expression with
  | Var name -> name
  | Num num -> string_of_int num
  | Bool bl -> string_of_bool bl
  | String str -> "\"" ^ str ^ "\""
  | Unop (op, exp) -> (
      match op with Negate -> "-" ^ exp_to_concrete_string exp)
  | Binop (bn, exp1, exp2) ->
      exp_to_concrete_string exp1
      ^ " " ^ get_concrete_binop bn ^ " "
      ^ exp_to_concrete_string exp2
  | Conditional (_if, _then, _else) ->
      "if " ^ exp_to_concrete_string _if ^ " then "
      ^ exp_to_concrete_string _then
      ^ " else "
      ^ exp_to_concrete_string _else
  | Fun (name, exp) -> "fun " ^ name ^ " -> " ^ exp_to_concrete_string exp
  | Let (name, value, _in) ->
      "let " ^ name ^ " = "
      ^ exp_to_concrete_string value
      ^ " in " ^ exp_to_concrete_string _in
  | Letrec (name, value, _in) ->
      "let rec " ^ name ^ " = "
      ^ exp_to_concrete_string value
      ^ " in " ^ exp_to_concrete_string _in
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (exp1, exp2) ->
      "("
      ^ exp_to_concrete_string exp1
      ^ ") " ^ "("
      ^ exp_to_concrete_string exp2
      ^ ")"

let get_abstract_binop (bn : binop) : string =
  match bn with
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Equals -> "Equals"
  | LessThan -> "LessThan"

(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (expression : expr) : string =
  match expression with
  | Var name -> "Var(" ^ name ^ ")"
  | Num num -> "Num(" ^ string_of_int num ^ ")"
  | Bool bl -> "Bool(" ^ string_of_bool bl ^ ")"
  | String str -> "String(" ^ str ^ ")"
  | Unop (op, exp) -> (
      match op with
      | Negate -> "Unop(" ^ "Negate, " ^ exp_to_concrete_string exp ^ ")")
  | Binop (bn, exp1, exp2) ->
      "Binop(" ^ get_abstract_binop bn ^ ", "
      ^ exp_to_abstract_string exp1
      ^ ", "
      ^ exp_to_abstract_string exp2
      ^ ")"
  | Conditional (_if, _else, _then) ->
      "Conditional(" ^ exp_to_abstract_string _if ^ ", "
      ^ exp_to_abstract_string _else
      ^ ", "
      ^ exp_to_abstract_string _then
      ^ ")"
  | Fun (name, exp) -> "Fun(" ^ name ^ ", " ^ exp_to_abstract_string exp ^ ")"
  | Let (name, exp1, exp2) ->
      "Let(" ^ name ^ ", "
      ^ exp_to_abstract_string exp1
      ^ ", "
      ^ exp_to_abstract_string exp2
      ^ ")"
  | Letrec (name, exp1, exp2) ->
      "Letrec(" ^ name ^ ", "
      ^ exp_to_abstract_string exp1
      ^ ", "
      ^ exp_to_abstract_string exp2
      ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (exp1, exp2) ->
      "App("
      ^ exp_to_abstract_string exp1
      ^ ", "
      ^ exp_to_abstract_string exp2
      ^ ")"
