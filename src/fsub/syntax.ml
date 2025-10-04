type ty =
  | TyTop
  | TyBool
  | TyNat
  | TyVar of string
  | TyArrow of ty * ty
  | TyForall of string * ty * ty

let rec string_of_ty = function
  | TyTop -> "Top"
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | TyVar v -> v
  | TyArrow (t1, t2) ->
      let lhs =
        match t1 with
        | TyArrow _ -> Printf.sprintf "(%s)" (string_of_ty t1)
        | _ -> string_of_ty t1
      in
      Printf.sprintf "%s -> %s" lhs (string_of_ty t2)
  | TyForall (v, bound, body) ->
      Printf.sprintf "forall %s <: %s. %s" v (string_of_ty bound) (string_of_ty body)

type term =
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmTyAbs of string * ty * term
  | TmTyApp of term * ty
  | TmBool of bool
  | TmIf of term * term * term
  | TmNat of int
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec string_of_term = function
  | TmVar x -> x
  | TmAbs (x, ty, body) ->
      Printf.sprintf "\\%s:%s. %s" x (string_of_ty ty) (string_of_term body)
  | TmApp (t1, t2) ->
      let s1 =
        match t1 with
        | TmApp _ -> Printf.sprintf "(%s)" (string_of_term t1)
        | _ -> string_of_term t1
      in
      let s2 =
        match t2 with
        | TmApp _ | TmAbs _ | TmTyAbs _ -> Printf.sprintf "(%s)" (string_of_term t2)
        | _ -> string_of_term t2
      in
      Printf.sprintf "%s %s" s1 s2
  | TmTyAbs (x, bound, body) ->
      Printf.sprintf "Λ%s <: %s. %s" x (string_of_ty bound) (string_of_term body)
  | TmTyApp (t, ty) ->
      let s =
        match t with
        | TmApp _ -> Printf.sprintf "(%s)" (string_of_term t)
        | _ -> string_of_term t
      in
      Printf.sprintf "%s[%s]" s (string_of_ty ty)
  | TmBool true -> "true"
  | TmBool false -> "false"
  | TmIf (t1, t2, t3) ->
      Printf.sprintf "if %s then %s else %s" (string_of_term t1) (string_of_term t2) (string_of_term t3)
  | TmNat n -> string_of_int n
  | TmSucc t -> Printf.sprintf "succ %s" (string_of_term t)
  | TmPred t -> Printf.sprintf "pred %s" (string_of_term t)
  | TmIsZero t -> Printf.sprintf "iszero %s" (string_of_term t)
