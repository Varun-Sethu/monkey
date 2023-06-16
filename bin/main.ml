open Sexplib.Std

(* NOTE: This file isnt actually part of the language
   this is just a small sketch playground for experiments :) *)

(* models arithmetic expressions *)
let (let+) = Option.bind

type parser = token list
and token =
  | Int of int
  | LParen | RParen
  | Plus | Mult
[@@deriving sexp]

type operand = 
  | Int of int
  | Plus of (operand * operand)
  | Mult of (operand * operand)
[@@deriving sexp]


let advance_parser = function | [] -> [] | _ :: xs -> xs
let peek_curr_token = function | [] -> None | x :: _ -> Some x
let token_is_op (token: token) = match token with | Plus | Mult -> true | _ -> false
let get_precedence (token: token) = 
  match token with
    | Int _ -> 1
    | Plus -> 2
    | Mult -> 3
    | RParen | LParen -> raise (Invalid_argument "precedences are not defined on parens")


let rec parse_atom ~(parser: parser) = 
  let+ curr_token = peek_curr_token parser in
  match curr_token with
    | Int x -> Some ((Int x), advance_parser parser)
    | LParen -> parse_tokens ~parser:(advance_parser parser) ~precedence:1
    | _ -> None

and construct_operand_from_token (token: token) lhs rhs = 
  match token with
    | Plus -> Some (Plus (lhs, rhs))
    | Mult -> Some (Mult (lhs, rhs))
    | _ -> None


and parse_tokens ~(parser: parser) ~precedence =
  let rec inner ~parser ~lhs =
    match peek_curr_token parser with
      | Some (RParen) | None -> Some (lhs, parser)
      | Some (curr_token) ->      
        let token_precedence = get_precedence curr_token in
        if (token_is_op curr_token) && (token_precedence >= precedence) then
            begin
              let next_min_prec = token_precedence + 1 in
              let+ rhs, parser = parse_tokens ~parser:(advance_parser parser) ~precedence:next_min_prec in
              let+ partial_operand = construct_operand_from_token curr_token lhs rhs in

              inner ~parser ~lhs:partial_operand
            end
        else Some (lhs, parser)
  in

  let+ atom, parser = parse_atom ~parser in
  inner ~parser ~lhs:atom


let () =
  let parsed_exr = parse_tokens ~parser:[Int 3; Mult; Int 4; Plus; Int 5] ~precedence:1 in
  match parsed_exr with
    | None -> print_endline "failed :("
    | Some (expr, _) ->
      let sexp = Base.Sexp.to_string (sexp_of_operand expr) in
      print_string sexp;
      print_newline () 

(* 3 + 4 + 5 -> 3 + (4 + 5) *)