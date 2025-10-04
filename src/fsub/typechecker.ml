open Syntax

exception Type_error of string

type term_ctx = (string * ty) list

type type_ctx = (string * ty) list

let rec lookup_term ctx x =
  match ctx with
  | [] ->
      let msg = Printf.sprintf "Unknown variable '%s'" x in
      raise (Type_error msg)
  | (y, ty) :: rest -> if String.equal x y then ty else lookup_term rest x

let rec lookup_type ctx x =
  match ctx with
  | [] -> None
  | (y, bound) :: rest -> if String.equal x y then Some bound else lookup_type rest x

let rec subst_ty ty var replacement =
  match ty with
  | TyTop -> TyTop
  | TyBool -> TyBool
  | TyNat -> TyNat
  | TyVar v -> if String.equal v var then replacement else ty
  | TyArrow (t1, t2) -> TyArrow (subst_ty t1 var replacement, subst_ty t2 var replacement)
  | TyForall (v, bound, body) ->
      let bound' = subst_ty bound var replacement in
      if String.equal v var then TyForall (v, bound', body)
      else TyForall (v, bound', subst_ty body var replacement)

let rec equal_ty t1 t2 =
  match t1, t2 with
  | TyTop, TyTop -> true
  | TyBool, TyBool -> true
  | TyNat, TyNat -> true
  | TyVar x, TyVar y -> String.equal x y
  | TyArrow (a1, b1), TyArrow (a2, b2) -> equal_ty a1 a2 && equal_ty b1 b2
  | TyForall (v1, bound1, body1), TyForall (v2, bound2, body2) ->
      equal_ty bound1 bound2
      &&
      let body2' = if String.equal v1 v2 then body2 else subst_ty body2 v2 (TyVar v1) in
      equal_ty body1 body2'
  | _ -> false

let rec check_type ctx ty =
  match ty with
  | TyTop | TyBool | TyNat -> ()
  | TyVar v ->
      begin
        match lookup_type ctx v with
        | Some _ -> ()
        | None ->
            let msg = Printf.sprintf "Unbound type variable '%s'" v in
            raise (Type_error msg)
      end
  | TyArrow (t1, t2) ->
      check_type ctx t1;
      check_type ctx t2
  | TyForall (v, bound, body) ->
      check_type ctx bound;
      check_type ((v, bound) :: ctx) body

let rec subtype ctx s t =
  if equal_ty s t then true
  else
    match s, t with
    | _, TyTop -> true
    | TyVar x, _ ->
        begin
          match lookup_type ctx x with
          | Some bound -> subtype ctx bound t
          | None -> false
        end
    | TyArrow (s1, s2), TyArrow (t1, t2) -> subtype ctx t1 s1 && subtype ctx s2 t2
    | TyForall (x1, bound1, body1), TyForall (x2, bound2, body2) ->
        subtype ctx bound2 bound1 &&
        let body2' = if String.equal x1 x2 then body2 else subst_ty body2 x2 (TyVar x1) in
        let ctx' = (x1, bound2) :: ctx in
        subtype ctx' body1 body2'
    | _ -> false

let expect_subtype ctx actual expected =
  if subtype ctx actual expected then ()
  else
    let msg =
      Printf.sprintf "Type mismatch: %s is not a subtype of %s"
        (string_of_ty actual) (string_of_ty expected)
    in
    raise (Type_error msg)

let rec infer_internal type_ctx term_ctx term =
  match term with
  | TmVar x -> lookup_term term_ctx x
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
            expect_subtype type_ctx ty_arg param_ty;
            ret_ty
        | _ ->
            let msg = Printf.sprintf "Expected a function but found %s" (string_of_ty ty_fun) in
            raise (Type_error msg)
      end
  | TmTyAbs (ty_param, bound, body) ->
      check_type type_ctx bound;
      let type_ctx' = (ty_param, bound) :: type_ctx in
      let body_ty = infer_internal type_ctx' term_ctx body in
      TyForall (ty_param, bound, body_ty)
  | TmTyApp (t, ty_arg) ->
      check_type type_ctx ty_arg;
      let ty_fun = infer_internal type_ctx term_ctx t in
      begin
        match ty_fun with
        | TyForall (ty_param, bound, body_ty) ->
            expect_subtype type_ctx ty_arg bound;
            subst_ty body_ty ty_param ty_arg
        | _ ->
            let msg = Printf.sprintf "Expected a polymorphic value but found %s"
                (string_of_ty ty_fun)
            in
            raise (Type_error msg)
      end
  | TmBool _ -> TyBool
  | TmIf (cond, t_then, t_else) ->
      let ty_cond = infer_internal type_ctx term_ctx cond in
      expect_subtype type_ctx ty_cond TyBool;
      let ty_then = infer_internal type_ctx term_ctx t_then in
      let ty_else = infer_internal type_ctx term_ctx t_else in
      if subtype type_ctx ty_then ty_else then ty_else
      else if subtype type_ctx ty_else ty_then then ty_then
      else
        let msg =
          Printf.sprintf "Branch types %s and %s are incompatible"
            (string_of_ty ty_then) (string_of_ty ty_else)
        in
        raise (Type_error msg)
  | TmNat _ -> TyNat
  | TmSucc t
  | TmPred t ->
      let ty_t = infer_internal type_ctx term_ctx t in
      expect_subtype type_ctx ty_t TyNat;
      TyNat
  | TmIsZero t ->
      let ty_t = infer_internal type_ctx term_ctx t in
      expect_subtype type_ctx ty_t TyNat;
      TyBool

let infer term = infer_internal [] [] term

let check term expected_ty =
  check_type [] expected_ty;
  let actual = infer_internal [] [] term in
  expect_subtype [] actual expected_ty
