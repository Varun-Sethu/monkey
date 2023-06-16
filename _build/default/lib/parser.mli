type t = ParserState.t

val new_parser : lexer:Lexer.t -> t

(* parse_program parses what could be a valid program from the token stream *)
val parse_program : parser:t -> Syntax.statement_node list option

(* parse_expression attempts to consume an expression from the parser *)
val parse_expression : parser:t -> (Syntax.expr_node * t) option

(* parse_statement attempts to consume a statement from the parser *)
val parse_statement : parser:t -> (Syntax.statement_node * t) option