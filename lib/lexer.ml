open Tokens

module SC = Set.Make(Char)
let (let+) = Option.bind

(* models the current state of the lexer *)
type t = 
  { 
    stream: string; 
    current_char: int; 
    current_line: int;
    current_column: int 
  }

let new_lexer ~stream = 
  { 
    stream;
    current_char = 0;
    current_line = 0;
    current_column = 0;
  }

let skip_curr_char ~lexer = { lexer with current_char = lexer.current_char + 1 }
let peek_curr_char ~lexer = if lexer.current_char < String.length lexer.stream then Some lexer.stream.[lexer.current_char]
                            else None

(* consumes the next char in the charachter stream *)
let consume_curr_char ~lexer = (peek_curr_char ~lexer, skip_curr_char ~lexer)

(* consumes from the token stream while a provided predicate remains true *)
let consume_while_true ~lexer ~predicate =
  let rec inner acc lexer =
    match peek_curr_char ~lexer with
      | None -> if acc = "" then (None, lexer) else (Some acc, lexer)
      | Some curr_char ->
          let should_consume_char = predicate curr_char in
          if should_consume_char then 
            inner (acc ^ String.make 1 curr_char) (skip_curr_char ~lexer)
          else (Some acc, lexer)
  in inner "" lexer

(* consumes from the token stream until we encounter an element in the provided set *)
let consume_till_elm_in_set ~set = 
  consume_while_true ~predicate:(fun c -> not (SC.exists ((=) c) set))

let is_whitespace c = SC.exists ((=) c) Tokens.whitespace_charachters

(* consumes the stream until we hit a reserved charachter *)
(* returns all the chars we consumed as a list of chars *)
let consume_till_reserved = consume_till_elm_in_set ~set:Tokens.reserved_charachters
let consume_till_char ~char = consume_till_elm_in_set ~set:(SC.singleton char)
let skip_whitespace ~lexer =
  let _, new_state = consume_while_true ~lexer ~predicate:is_whitespace in
  new_state



(* tokenises an actual operator/symbol
   symbols just correspond to single charachter values ie. '(' *)
let tokenise_symbol ~lexer =
  let old_lexer = lexer in
  let lexer = skip_whitespace ~lexer in
  let current_char, lexer = consume_curr_char ~lexer in
  match current_char with
    | Some '=' when peek_curr_char ~lexer = Some '>' -> (Some (Arrow), skip_curr_char ~lexer)
    | Some '|' when peek_curr_char ~lexer = Some '|' -> (Some (Operator Or), skip_curr_char ~lexer)
    | Some '&' when peek_curr_char ~lexer = Some '&' -> (Some (Operator And), skip_curr_char ~lexer)
    | Some '=' when peek_curr_char ~lexer = Some '=' -> (Some (Operator Equality), skip_curr_char ~lexer)
    | Some '!' when peek_curr_char ~lexer = Some '=' -> (Some (Operator Inequality), skip_curr_char ~lexer)
    | Some '<' when peek_curr_char ~lexer = Some '=' -> (Some (Operator LessThanOrEq), skip_curr_char ~lexer)
    | Some '>' when peek_curr_char ~lexer = Some '=' -> (Some (Operator GreaterThanOrEq), skip_curr_char ~lexer)
    | Some '+' -> (Some (Operator Add), lexer)
    | Some '-' -> (Some (Operator Sub), lexer)
    | Some '*' -> (Some (Operator Mult), lexer)
    | Some '/' -> (Some (Operator Div), lexer)
    | Some '%' -> (Some (Operator Rem), lexer)
    | Some '!' -> (Some (Operator Not), lexer)
    | Some '=' -> (Some Assignment, lexer)
    | Some ';' -> (Some Semicolon, lexer)
    | Some '[' -> (Some LSquare, lexer)
    | Some ']' -> (Some RSquare, lexer)
    | Some '(' -> (Some LParen, lexer)
    | Some ')' -> (Some RParen, lexer)
    | Some '{' -> (Some LCurly, lexer)
    | Some '}' -> (Some RCurly, lexer)
    | Some ',' -> (Some Comma, lexer)
    | Some ':' -> (Some Colon, lexer)
    | _ -> (None, old_lexer)


(* tokenises and categorieses "words", ie if, let, fn, let *)
let tokenise_word ~lexer =
  let old_lexer = lexer in
  let lexer = skip_whitespace ~lexer in
  let next_word, lexer = consume_till_reserved ~lexer in
  match next_word with
    | Some "let"   -> (Some Let, lexer)
    | Some "if"    -> (Some If, lexer)
    | Some "else"  -> (Some Else, lexer)
    | Some "then"  -> (Some Then, lexer)
    | Some "fn"    -> (Some Fn, lexer)
    | Some "true"  -> (Some (Literal (Bool true)), lexer)
    | Some "false" -> (Some (Literal (Bool false)), lexer)
    | _ -> (None, old_lexer)


(* small logic for dealing with literals *)
let integer_regex = Str.regexp {|^[0-9]+$|}
(* takes a raw literal and attemps to parse it into a literal token *)
let parse_literal literal =
  let+ unparsed_literal = literal in
  if Str.string_match integer_regex unparsed_literal 0 then
    Some (Literal (Int (int_of_string unparsed_literal)))
  else Some (Variable unparsed_literal)


(* tokenise_literal attempts to tokenise the next set of chars as a literal *)
let tokenise_literal ~lexer =
  let lexer = skip_whitespace ~lexer in
  match peek_curr_char ~lexer with
  | Some '"' -> 
    let consumed_string, lexer = consume_till_char ~lexer:(skip_curr_char ~lexer) ~char:'"' in
    let quoted_string = Option.bind consumed_string (fun string -> Some (Literal (String string))) in
    (quoted_string, skip_curr_char ~lexer)
  | Some x when not (is_whitespace x) -> 
    let literal, lexer = consume_till_reserved ~lexer in (parse_literal literal, lexer)
  | _ -> (None, lexer)



(* produces the next token within the stream pointed at by lexer *)
(* iterates over a list of tokenisers and returns the result of the first one that doesnt
   return None *)
let next_token ~lexer = 
  let tokenisers = [| tokenise_symbol; tokenise_word; tokenise_literal |] in
  let rec inner current_tokeniser lexer =
    if current_tokeniser = Array.length tokenisers then (None, lexer)
    else
      let (next_attempt, lexer) = tokenisers.(current_tokeniser) ~lexer in
      if Option.is_some next_attempt then (next_attempt, skip_whitespace ~lexer)
      else inner (current_tokeniser + 1) lexer
  in inner 0 lexer

(* produces a list of tokens to consume from a lexer stream *)
let rec consume_all_tokens ~lexer = 
  let (consumed_token, new_lexer) = next_token ~lexer in
  match consumed_token with
    | None -> []
    | Some consumed_token ->
      let rem_tokens = consume_all_tokens ~lexer:new_lexer in
      [ consumed_token ] @ rem_tokens

(* == TESTS == *)
let%test "tokenises_no_whitespace" = 
  let example_program = {|func(3, "hello", "tomato")|} in
  let lexer = new_lexer ~stream:example_program in
  let tokens = consume_all_tokens ~lexer in
  List.iter (fun token ->
    let sexp = Base.Sexp.to_string (sexp_of_token token) in
    print_string sexp;
    print_newline ()
  ) tokens;

  true