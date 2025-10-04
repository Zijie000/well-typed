open Syntax

exception Type_error of string

type context = (string * ty) list

let rec lookup ctx x =
  match ctx with
  | [] ->
      let msg = Printf.sprintf "Unknown variable '%s'" x in
      raise (Type_error msg)
  | (y, ty) :: rest -> if String.equal x y then ty else lookup rest x

let rec equal_ty t1 t2 =
  match t1, t2 with
  | TyBool, TyBool -> true
  | TyNat, TyNat -> true
  | TyArrow (a1, b1), TyArrow (a2, b2) -> equal_ty a1 a2 && equal_ty b1 b2
  | _ -> false

let expect_ty expected actual =
  if equal_ty expected actual then ()
  else
    let msg =
      Printf.sprintf "Type mismatch: expected %s but found %s"
        (string_of_ty expected) (string_of_ty actual)
    in
    raise (Type_error msg)

let rec infer ctx term =
  match term with
  | TmVar x -> lookup ctx x
  | TmAbs (param, ty_param, body) ->
      let ctx' = (param, ty_param) :: ctx in
      let ty_body = infer ctx' body in
      TyArrow (ty_param, ty_body)
  | TmApp (t1, t2) ->
      let ty_fun = infer ctx t1 in
      let ty_arg = infer ctx t2 in
      begin
        match ty_fun with
        | TyArrow (param_ty, ret_ty) ->
            expect_ty param_ty ty_arg;
            ret_ty
        | _ ->
            let msg = Printf.sprintf "Expected a function but found %s" (string_of_ty ty_fun) in
            raise (Type_error msg)
      end
  | TmBool _ -> TyBool
  | TmIf (cond, t_then, t_else) ->
      let ty_cond = infer ctx cond in
      expect_ty TyBool ty_cond;
      let ty_then = infer ctx t_then in
      let ty_else = infer ctx t_else in
      if equal_ty ty_then ty_else then ty_then
      else
        let msg =
          Printf.sprintf "Branches of conditional have different types: %s vs %s"
            (string_of_ty ty_then) (string_of_ty ty_else)
        in
        raise (Type_error msg)
  | TmNat _ -> TyNat
  | TmSucc t
  | TmPred t ->
      let ty_t = infer ctx t in
      expect_ty TyNat ty_t;
      TyNat
  | TmIsZero t ->
      let ty_t = infer ctx t in
      expect_ty TyNat ty_t;
      TyBool

let check term expected_ty =
  let actual = infer [] term in
  expect_ty expected_ty actual
