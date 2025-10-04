open Syntax

exception Parse_error of string

type token =
  | LParen
  | RParen
  | Lambda
  | Dot
  | Colon
  | Arrow
  | If
  | Then
  | Else
  | True
  | False
  | Succ
  | Pred
  | IsZero
  | TyBool
  | TyNat
  | Ident of string
  | Int of int
  | EOF

let is_space = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_char c = is_ident_start c || is_digit c || c = '\''

let classify_word w =
  match String.lowercase_ascii w with
  | "if" -> If
  | "then" -> Then
  | "else" -> Else
  | "true" -> True
  | "false" -> False
  | "succ" -> Succ
  | "pred" -> Pred
  | "iszero" -> IsZero
  | "lambda" -> Lambda
  | "bool" -> TyBool
  | "nat" -> TyNat
  | _ -> Ident w

let lex input =
  let len = String.length input in
  let rec aux i acc =
    if i >= len then List.rev (EOF :: acc)
    else
      match input.[i] with
      | c when is_space c -> aux (i + 1) acc
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | ':' -> aux (i + 1) (Colon :: acc)
      | '.' -> aux (i + 1) (Dot :: acc)
      | '\\' -> aux (i + 1) (Lambda :: acc)
      | '-' when i + 1 < len && input.[i + 1] = '>' -> aux (i + 2) (Arrow :: acc)
      | c when is_digit c ->
          let j = ref i in
          while !j < len && is_digit input.[!j] do
            incr j
          done;
          let value = int_of_string (String.sub input i (!j - i)) in
          aux !j (Int value :: acc)
      | c when is_ident_start c ->
          let j = ref i in
          while !j < len && is_ident_char input.[!j] do
            incr j
          done;
          let word = String.sub input i (!j - i) in
          aux !j (classify_word word :: acc)
      | other ->
          let msg = Printf.sprintf "Unexpected character '%c'" other in
          raise (Parse_error msg)
  in
  aux 0 []

let peek = function [] -> EOF | tok :: _ -> tok

let expect tok tokens =
  match tokens with
  | x :: rest when x = tok -> rest
  | _ -> raise (Parse_error "Unexpected token")

let rec parse_type tokens = parse_arrow_type tokens

and parse_arrow_type tokens =
  let lhs, rest = parse_atomic_type tokens in
  match rest with
  | Arrow :: rest' ->
      let rhs, rest'' = parse_arrow_type rest' in
      (TyArrow (lhs, rhs), rest'')
  | _ -> (lhs, rest)

and parse_atomic_type = function
  | TyBool :: rest -> (TyBool, rest)
  | TyNat :: rest -> (TyNat, rest)
  | LParen :: rest ->
      let ty, rest' = parse_type rest in
      let rest'' =
        match rest' with
        | RParen :: tail -> tail
        | _ -> raise (Parse_error "Expected closing parenthesis in type")
      in
      (ty, rest'')
  | Ident name :: _ ->
      let msg = Printf.sprintf "Unknown type '%s'" name in
      raise (Parse_error msg)
  | tok :: _ ->
      let msg = Printf.sprintf "Unexpected token while parsing type: %s" (token_to_string tok) in
      raise (Parse_error msg)
  | [] -> raise (Parse_error "Unexpected end of input when parsing type")

and token_to_string = function
  | LParen -> "("
  | RParen -> ")"
  | Lambda -> "λ"
  | Dot -> "."
  | Colon -> ":"
  | Arrow -> "->"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | True -> "true"
  | False -> "false"
  | Succ -> "succ"
  | Pred -> "pred"
  | IsZero -> "iszero"
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | Ident s -> s
  | Int n -> string_of_int n
  | EOF -> "<eof>"

let rec parse_term tokens =
  match peek tokens with
  | If -> parse_if tokens
  | Lambda -> parse_lambda tokens
  | _ -> parse_application tokens

and parse_if tokens =
  match tokens with
  | If :: rest ->
      let cond, rest1 = parse_term rest in
      begin
        match rest1 with
        | Then :: rest2 ->
            let t_then, rest3 = parse_term rest2 in
            begin
              match rest3 with
              | Else :: rest4 ->
                  let t_else, rest5 = parse_term rest4 in
                  (TmIf (cond, t_then, t_else), rest5)
              | _ -> raise (Parse_error "Expected 'else' keyword")
            end
        | _ -> raise (Parse_error "Expected 'then' keyword")
      end
  | _ -> raise (Parse_error "Expected 'if'")

and parse_lambda = function
  | Lambda :: Ident param :: Colon :: rest ->
      let ty, rest1 = parse_type rest in
      begin
        match rest1 with
        | Dot :: rest2 ->
            let body, rest3 = parse_term rest2 in
            (TmAbs (param, ty, body), rest3)
        | _ -> raise (Parse_error "Expected '.' after lambda parameter")
      end
  | Lambda :: _ -> raise (Parse_error "Malformed lambda abstraction")
  | _ -> raise (Parse_error "Expected lambda")

and parse_application tokens =
  let head, rest = parse_atomic tokens in
  parse_application_tail head rest

and parse_application_tail head tokens =
  match peek tokens with
  | Ident _
  | Int _
  | True
  | False
  | LParen
  | Lambda
  | Succ
  | Pred
  | IsZero ->
      let arg, rest1 = parse_atomic tokens in
      parse_application_tail (TmApp (head, arg)) rest1
  | _ -> (head, tokens)

and parse_atomic tokens =
  match tokens with
  | True :: rest -> (TmBool true, rest)
  | False :: rest -> (TmBool false, rest)
  | Int n :: rest -> (TmNat n, rest)
  | Ident name :: rest -> (TmVar name, rest)
  | Succ :: rest ->
      let arg, rest1 = parse_atomic rest in
      (TmSucc arg, rest1)
  | Pred :: rest ->
      let arg, rest1 = parse_atomic rest in
      (TmPred arg, rest1)
  | IsZero :: rest ->
      let arg, rest1 = parse_atomic rest in
      (TmIsZero arg, rest1)
  | LParen :: rest ->
      let term, rest1 = parse_term rest in
      begin
        match rest1 with
        | RParen :: rest2 -> (term, rest2)
        | _ -> raise (Parse_error "Expected closing parenthesis")
      end
  | (Lambda :: _) as toks -> parse_lambda toks
  | tok :: _ ->
      let msg = Printf.sprintf "Unexpected token '%s'" (token_to_string tok) in
      raise (Parse_error msg)
  | [] -> raise (Parse_error "Unexpected end of input")

let parse input =
  let tokens = lex input in
  let term, rest = parse_term tokens in
  match rest with
  | [EOF] -> term
  | EOF :: _ -> term
  | tok :: _ ->
      let msg = Printf.sprintf "Unexpected extra input near '%s'" (token_to_string tok) in
      raise (Parse_error msg)
  | [] -> term

let parse_type_string input =
  let tokens = lex input in
  let ty, rest = parse_type tokens in
  match rest with
  | [EOF] | [EOF; EOF] -> ty
  | EOF :: _ -> ty
  | tok :: _ ->
      let msg = Printf.sprintf "Unexpected extra input near '%s'" (token_to_string tok) in
      raise (Parse_error msg)
  | [] -> ty
