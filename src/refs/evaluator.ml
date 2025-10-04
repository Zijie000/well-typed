open Syntax

exception Runtime_error of string

module IntOrd = struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make (IntOrd)

type loc = int

type value =
  | VBool of bool
  | VNat of int
  | VUnit
  | VClosure of string * term * env
  | VLoc of loc

and env = (string * value) list

type state = {
  store : value IntMap.t;
  next_loc : int;
}

let empty_state = { store = IntMap.empty; next_loc = 0 }

let rec lookup_env env x =
  match env with
  | [] ->
      let msg = Printf.sprintf "Unbound variable '%s'" x in
      raise (Runtime_error msg)
  | (y, v) :: rest -> if String.equal x y then v else lookup_env rest x

let lookup_store state loc =
  try IntMap.find loc state.store
  with Not_found ->
    let msg = Printf.sprintf "Invalid location %d" loc in
    raise (Runtime_error msg)

let alloc state value =
  let loc = state.next_loc in
  let store = IntMap.add loc value state.store in
  let state' = { store; next_loc = loc + 1 } in
  (loc, state')

let update_store state loc value =
  if IntMap.mem loc state.store then { state with store = IntMap.add loc value state.store }
  else
    let msg = Printf.sprintf "Invalid location %d" loc in
    raise (Runtime_error msg)

let rec eval env state term =
  match term with
  | TmVar x -> (lookup_env env x, state)
  | TmAbs (param, _, body) -> (VClosure (param, body, env), state)
  | TmApp (t1, t2) ->
      let v_fun, state1 = eval env state t1 in
      let v_arg, state2 = eval env state1 t2 in
      begin
        match v_fun with
        | VClosure (param, body, closure_env) ->
            let env' = (param, v_arg) :: closure_env in
            eval env' state2 body
        | _ -> raise (Runtime_error "Attempting to apply a non-function value")
      end
  | TmBool b -> (VBool b, state)
  | TmIf (cond, t_then, t_else) ->
      let v_cond, state1 = eval env state cond in
      begin
        match v_cond with
        | VBool true -> eval env state1 t_then
        | VBool false -> eval env state1 t_else
        | _ -> raise (Runtime_error "Condition of if must be a boolean")
      end
  | TmNat n -> (VNat n, state)
  | TmSucc t ->
      let v, state1 = eval env state t in
      begin
        match v with
        | VNat n -> (VNat (n + 1), state1)
        | _ -> raise (Runtime_error "succ expects a Nat argument")
      end
  | TmPred t ->
      let v, state1 = eval env state t in
      begin
        match v with
        | VNat n -> (VNat (if n = 0 then 0 else n - 1), state1)
        | _ -> raise (Runtime_error "pred expects a Nat argument")
      end
  | TmIsZero t ->
      let v, state1 = eval env state t in
      begin
        match v with
        | VNat n -> (VBool (n = 0), state1)
        | _ -> raise (Runtime_error "iszero expects a Nat argument")
      end
  | TmUnit -> (VUnit, state)
  | TmRef t ->
      let v, state1 = eval env state t in
      let loc, state2 = alloc state1 v in
      (VLoc loc, state2)
  | TmDeref t ->
      let v, state1 = eval env state t in
      begin
        match v with
        | VLoc loc -> (lookup_store state1 loc, state1)
        | _ -> raise (Runtime_error "! expects a reference")
      end
  | TmAssign (t_ref, t_val) ->
      let v_ref, state1 = eval env state t_ref in
      let v_val, state2 = eval env state1 t_val in
      begin
        match v_ref with
        | VLoc loc -> (VUnit, update_store state2 loc v_val)
        | _ -> raise (Runtime_error ":= expects a reference on the left-hand side")
      end

let eval term =
  let value, _state = eval [] empty_state term in
  value

let string_of_value = function
  | VBool true -> "true"
  | VBool false -> "false"
  | VNat n -> string_of_int n
  | VUnit -> "unit"
  | VClosure _ -> "<fun>"
  | VLoc loc -> Printf.sprintf "loc[%d]" loc
