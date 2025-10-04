module Syntax = Syntax
module Parser = Parser
module Typechecker = Typechecker
module Evaluator = Evaluator

let parse = Parser.parse
let parse_type = Parser.parse_type_string
let infer = Typechecker.infer
let eval = Evaluator.eval
