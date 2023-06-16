
open Tokens

type t = { lexer: Lexer.t; curr_token: token option; next_token: token option }

(* new_parser constructs a brand new parser given a lexer backing it *)
let new_parser ~lexer =
  let curr_token, lexer = Lexer.next_token ~lexer in
  let next_token, lexer = Lexer.next_token ~lexer in
  { 
    lexer; 
    curr_token; 
    next_token
  }
    
    
(* advance_parser shifts the current token the parser is looking at down *)
let advance_parser ~parser =
  let curr_token = parser.next_token in
  let next_token, lexer = Lexer.next_token ~lexer:parser.lexer in
  { 
    lexer; 
    curr_token; 
    next_token 
  }

(* is_empty determines if the parser has fully consumed its token stream *)
let is_empty ~parser = 
  parser.next_token = None && parser.curr_token = None

  
(* skip skips the n tokens int he parser stream *)
let rec skip ~parser ~n = 
  if n = 0 then parser
  else
    skip ~n:(n - 1) ~parser:(advance_parser ~parser)


(* peek_current/peek_next peeks the current and next token in the stream accordingly *)
let peek_current ~parser = parser.curr_token
let peek_next ~parser = parser.next_token


(* consume_current consumes the current token and progresses the stream *)
let consume_current ~parser =
  let new_parser = advance_parser ~parser in
  peek_current ~parser, new_parser

  
(* skip_parser_in_result skips the parser part of a parse result *)
let skip_parser_in_result ~result =
  match result with
    | None -> None
    | Some (result, parser) ->
      Some (result, advance_parser ~parser)