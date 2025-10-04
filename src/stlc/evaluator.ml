open Syntax

exception Runtime_error of string

type value =
  | VBool of bool
  | VNat of int
  | VClosure of string * term * env

and env = (string * value) list

let rec lookup env x =
  match env with
  | [] ->
      let msg = Printf.sprintf "Unbound variable '%s'" x in
      raise (Runtime_error msg)
  | (y, v) :: rest -> if String.equal x y then v else lookup rest x

let rec eval env term =
  match term with
  | TmVar x -> lookup env x
  | TmAbs (param, _, body) -> VClosure (param, body, env)
  | TmApp (t1, t2) ->
      let v_fun = eval env t1 in
      let v_arg = eval env t2 in
      begin
        match v_fun with
        | VClosure (param, body, closure_env) ->
            let env' = (param, v_arg) :: closure_env in
            eval env' body
        | _ -> raise (Runtime_error "Attempting to apply a non-function value")
      end
  | TmBool b -> VBool b
  | TmIf (cond, t_then, t_else) ->
      begin
        match eval env cond with
        | VBool true -> eval env t_then
        | VBool false -> eval env t_else
        | _ -> raise (Runtime_error "Condition of if must be a boolean")
      end
  | TmNat n -> VNat n
  | TmSucc t ->
      begin
        match eval env t with
        | VNat n -> VNat (n + 1)
        | _ -> raise (Runtime_error "succ expects a Nat argument")
      end
  | TmPred t ->
      begin
        match eval env t with
        | VNat n -> VNat (if n = 0 then 0 else n - 1)
        | _ -> raise (Runtime_error "pred expects a Nat argument")
      end
  | TmIsZero t ->
      begin
        match eval env t with
        | VNat n -> VBool (n = 0)
        | _ -> raise (Runtime_error "iszero expects a Nat argument")
      end

let eval term = eval [] term

let string_of_value = function
  | VBool true -> "true"
  | VBool false -> "false"
  | VNat n -> string_of_int n
  | VClosure _ -> "<fun>"
