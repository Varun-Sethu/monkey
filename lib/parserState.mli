type t
val new_parser : lexer:Lexer.t -> t

(* advance_parser shifts the current token the parser is looking at down *)
val advance_parser : parser:t -> t

(* is_empty determines if the parser has fully consumed its token stream *)
val is_empty : parser:t -> bool

(* skip skips the n tokens int he parser stream *)
val skip : parser:t -> n:int -> t

(* peek_current/peek_next returns the current token the parser is looking at *)
val peek_current : parser:t -> Tokens.token option
val peek_next : parser:t -> Tokens.token option

(* consume_current consumes the current token and progresses the stream *)
val consume_current : parser:t -> (Tokens.token option * t)

(* skip_parser_in_result skips the parser part of a parse result *)
val skip_parser_in_result : result:('a * t) option -> ('a * t) option

