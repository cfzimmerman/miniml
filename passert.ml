let passert ?(suite = "") (cond : bool) (message : string) =
  try
    assert cond;
    print_endline
      ("✅ " ^ (if String.length suite > 0 then suite ^ ": " else "") ^ message)
  with _ ->
    print_endline ("❌ FAILED " ^ message);
    raise (Failure "Test failed")

(* Calls a function that's supposed to fail. Returns false if it doesn't. *)
let should_fail (f : 'a -> 'b) (arg : 'a) : bool =
  try
    let _ = f arg in
    false
  with _ -> true
