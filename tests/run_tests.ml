let test_stlc () =
  let open Stlc in
  let term = parse "(\\x:Bool. x) true" in
  let ty = infer term in
  let ty_str = Syntax.string_of_ty ty in
  assert (String.equal ty_str "Bool");
  let value = eval term in
  assert (String.equal (Evaluator.string_of_value value) "true");
  let bad_term = parse "(\\x:Bool. x) 0" in
  try
    let _ = Typechecker.infer [] bad_term in
    failwith "Expected STLC type error"
  with
  | Typechecker.Type_error _ -> ()

let test_systemf () =
  let open Systemf in
  let term = parse "(biglambda X. \\x:X. x)[Bool] true" in
  let ty = infer term in
  assert (String.equal (Syntax.string_of_ty ty) "Bool");
  let value = eval term in
  assert (String.equal (Evaluator.string_of_value value) "true");
  let bad_term = parse "((biglambda X. \\x:X. x)[Bool]) 0" in
  (try
     let _ = infer bad_term in
     failwith "Expected System F type error"
   with
   | Typechecker.Type_error _ -> ());
  ()

let test_fsub () =
  let open Fsub in
  let term = parse "(biglambda X <: Top. \\x:X. x)[Bool] true" in
  let ty = infer term in
  assert (String.equal (Syntax.string_of_ty ty) "Bool");
  let value = eval term in
  assert (String.equal (Evaluator.string_of_value value) "true");
  let bad_term = parse "(biglambda X <: Bool. \\x:X. x)[Nat]" in
  (try
     let _ = infer bad_term in
     failwith "Expected F<: type error"
   with
   | Typechecker.Type_error _ -> ());
  let subtyping_term = parse "(\\x:Top. x) true" in
  let ty_sub = infer subtyping_term in
  assert (String.equal (Syntax.string_of_ty ty_sub) "Top");
  ()

let test_refs () =
  let open Refs in
  let term =
    parse "(\\r:Ref Nat. (\\x:Unit. !r) (r := succ !r)) (ref 0)" in
  let ty = infer term in
  assert (String.equal (Syntax.string_of_ty ty) "Nat");
  let value = eval term in
  assert (String.equal (Evaluator.string_of_value value) "1");
  let deref_term = parse "!(ref 0)" in
  let value_zero = eval deref_term in
  assert (String.equal (Evaluator.string_of_value value_zero) "0");
  let bad_term = parse "(\\r:Ref Nat. r := true) (ref 0)" in
  (try
     let _ = infer bad_term in
     failwith "Expected Ref type error"
   with
   | Typechecker.Type_error _ -> ());
  ()

let () =
  test_stlc ();
  test_systemf ();
  test_fsub ();
  test_refs ();
  print_endline "All tests passed."
