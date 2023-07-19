open Sexplib.Std

(* models actual literal values and their inferred type *)
(* TODO: deal with potential out of bound reads *)
type literal = 
  | Int of (int [@compare.ignore])
  | String of (string [@compare.ignore])
  | Bool of (bool [@compare.ignore])
[@@deriving sexp, compare]

(* models a physical operator in the programming language *)
(* TODO: logical conjuction operators *)
type operator = 
  | Equality
  | Inequality
  | LessThanOrEq
  | GreaterThanOrEq
  | LessThan
  | GreaterThan
  | Add
  | Sub
  | Mult
  | Div
  | Rem
  | And
  | Or
  | Not
  | Xor
[@@deriving sexp, compare]

(* tokens for the monkey programming language *)
type token = 
  | LParen
  | RParen
  | LCurly
  | RCurly
  | LSquare
  | RSquare
  | Comma
  | Semicolon
  | Arrow
  | Colon
  | Let
  | If
  | Then
  | Else
  | Assignment
  | Fn
  | Operator of operator
  | Literal of literal
  | Variable of (string [@compare.ignore])
[@@deriving sexp, compare]

(* contains information regarding where this token was consumed from the file stream *)
type token_properties = 
  {
    token: token;
    line: int; [@compare.ignore]
    column: int [@compare.ignore]
  }
[@@deriving sexp, compare]

let whitespace_charachters = 
  let open (Set.Make(Char)) in 
    of_list [
      '\n'; ' '; '\t'
    ]

(* set of all reserved charachters, no identifier can contain any of these chars *)
let reserved_charachters = 
  let open (Set.Make(Char)) in
    union 
      whitespace_charachters 
      (of_list [
        '='; '+'; '-'; '/'; '*'; '%'; '"'; '\''; ','; '<'; '>'; '('; ')';
        '['; ']'; '{'; '}'; '.'; ';'; ':'; ' '
      ])

(* determines of a provided token is an operator *)
let token_is_operator ~token =
  match token with
    | Operator _ -> true
    | _ -> false