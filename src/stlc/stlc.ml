module Syntax = Syntax
module Parser = Parser
module Typechecker = Typechecker
module Evaluator = Evaluator

let parse = Parser.parse
let parse_type = Parser.parse_type_string
let infer term = Typechecker.infer [] term
let eval = Evaluator.eval
