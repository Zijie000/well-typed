open Syntax

exception Type_error of string

type term_ctx = (string * ty) list

let rec lookup ctx x =
  match ctx with
  | [] ->
      let msg = Printf.sprintf "Unknown variable '%s'" x in
      raise (Type_error msg)
  | (y, ty) :: rest -> if String.equal x y then ty else lookup rest x

let rec subst_ty ty var replacement =
  match ty with
  | TyBool -> TyBool
  | TyNat -> TyNat
  | TyVar v -> if String.equal v var then replacement else ty
  | TyArrow (t1, t2) -> TyArrow (subst_ty t1 var replacement, subst_ty t2 var replacement)
  | TyForall (v, body) ->
      if String.equal v var then ty
      else TyForall (v, subst_ty body var replacement)

let rec equal_ty t1 t2 =
  match t1, t2 with
  | TyBool, TyBool -> true
  | TyNat, TyNat -> true
  | TyVar a, TyVar b -> String.equal a b
  | TyArrow (a1, b1), TyArrow (a2, b2) -> equal_ty a1 a2 && equal_ty b1 b2
  | TyForall (v1, body1), TyForall (v2, body2) ->
      if String.equal v1 v2 then equal_ty body1 body2
      else
        let body2' = subst_ty body2 v2 (TyVar v1) in
        equal_ty body1 body2'
  | _ -> false

type type_ctx = string list

let rec type_var_bound ctx v =
  match ctx with
  | [] -> false
  | x :: rest -> String.equal x v || type_var_bound rest v

let rec check_type ctx = function
  | TyBool | TyNat -> ()
  | TyVar v ->
      if type_var_bound ctx v then ()
      else
        let msg = Printf.sprintf "Unbound type variable '%s'" v in
        raise (Type_error msg)
  | TyArrow (t1, t2) ->
      check_type ctx t1;
      check_type ctx t2
  | TyForall (v, body) ->
      check_type (v :: ctx) body

let expect_ty expected actual =
  if equal_ty expected actual then ()
  else
    let msg =
      Printf.sprintf "Type mismatch: expected %s but found %s"
        (string_of_ty expected) (string_of_ty actual)
    in
    raise (Type_error msg)

let rec infer_internal type_ctx term_ctx term =
  match term with
  | TmVar x -> lookup term_ctx x
  | TmAbs (param, param_ty, body) ->
      check_type type_ctx param_ty;
      let term_ctx' = (param, param_ty) :: term_ctx in
      let body_ty = infer_internal type_ctx term_ctx' body in
      TyArrow (param_ty, body_ty)
  | TmApp (t1, t2) ->
      let ty_fun = infer_internal type_ctx term_ctx t1 in
      let ty_arg = infer_internal type_ctx term_ctx t2 in
      begin
        match ty_fun with
        | TyArrow (param_ty, ret_ty) ->
            expect_ty param_ty ty_arg;
            ret_ty
        | _ ->
            let msg = Printf.sprintf "Expected a function but found %s" (string_of_ty ty_fun) in
            raise (Type_error msg)
      end
  | TmTyAbs (ty_param, body) ->
      let type_ctx' = ty_param :: type_ctx in
      let body_ty = infer_internal type_ctx' term_ctx body in
      TyForall (ty_param, body_ty)
  | TmTyApp (t, ty_arg) ->
      check_type type_ctx ty_arg;
      let ty_fun = infer_internal type_ctx term_ctx t in
      begin
        match ty_fun with
        | TyForall (ty_param, body_ty) -> subst_ty body_ty ty_param ty_arg
        | _ ->
            let msg = Printf.sprintf "Expected a polymorphic value but found %s"
                (string_of_ty ty_fun)
            in
            raise (Type_error msg)
      end
  | TmBool _ -> TyBool
  | TmIf (cond, t_then, t_else) ->
      let ty_cond = infer_internal type_ctx term_ctx cond in
      expect_ty TyBool ty_cond;
      let ty_then = infer_internal type_ctx term_ctx t_then in
      let ty_else = infer_internal type_ctx term_ctx t_else in
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
      let ty_t = infer_internal type_ctx term_ctx t in
      expect_ty TyNat ty_t;
      TyNat
  | TmIsZero t ->
      let ty_t = infer_internal type_ctx term_ctx t in
      expect_ty TyNat ty_t;
      TyBool

let infer term = infer_internal [] [] term

let check term expected_ty =
  check_type [] expected_ty;
  let actual = infer_internal [] [] term in
  expect_ty expected_ty actual
