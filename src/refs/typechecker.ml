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
  | TyUnit, TyUnit -> true
  | TyArrow (a1, b1), TyArrow (a2, b2) -> equal_ty a1 a2 && equal_ty b1 b2
  | TyRef ty1, TyRef ty2 -> equal_ty ty1 ty2
  | _ -> false

let expect_ty expected actual =
  if equal_ty expected actual then ()
  else
    let msg =
      Printf.sprintf "Type mismatch: expected %s but found %s"
        (string_of_ty expected) (string_of_ty actual)
    in
    raise (Type_error msg)

let rec infer_with_ctx ctx term =
  match term with
  | TmVar x -> lookup ctx x
  | TmAbs (param, param_ty, body) ->
      let ctx' = (param, param_ty) :: ctx in
      let body_ty = infer_with_ctx ctx' body in
      TyArrow (param_ty, body_ty)
  | TmApp (t1, t2) ->
      let ty_fun = infer_with_ctx ctx t1 in
      let ty_arg = infer_with_ctx ctx t2 in
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
      let ty_cond = infer_with_ctx ctx cond in
      expect_ty TyBool ty_cond;
      let ty_then = infer_with_ctx ctx t_then in
      let ty_else = infer_with_ctx ctx t_else in
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
      let ty_t = infer_with_ctx ctx t in
      expect_ty TyNat ty_t;
      TyNat
  | TmIsZero t ->
      let ty_t = infer_with_ctx ctx t in
      expect_ty TyNat ty_t;
      TyBool
  | TmUnit -> TyUnit
  | TmRef t ->
      let ty_t = infer_with_ctx ctx t in
      TyRef ty_t
  | TmDeref t ->
      begin
        match infer_with_ctx ctx t with
        | TyRef ty_inner -> ty_inner
        | other ->
            let msg = Printf.sprintf "Expected a reference but found %s" (string_of_ty other) in
            raise (Type_error msg)
      end
  | TmAssign (t1, t2) ->
      begin
        match infer_with_ctx ctx t1 with
        | TyRef ty_inner ->
            let ty_rhs = infer_with_ctx ctx t2 in
            expect_ty ty_inner ty_rhs;
            TyUnit
        | other ->
            let msg = Printf.sprintf "Left side of assignment must be a reference, found %s"
                (string_of_ty other)
            in
            raise (Type_error msg)
      end

let infer term = infer_with_ctx [] term

let check term expected_ty =
  let actual = infer_with_ctx [] term in
  expect_ty expected_ty actual
