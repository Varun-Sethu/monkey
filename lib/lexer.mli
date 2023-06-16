type t

(* new_lexer constructs a new lexer to use for parsing a stream *)
val new_lexer : stream:string -> t

(* next_token returns the next token in the lexer stream *)
val next_token : lexer:t -> Tokens.token option * t