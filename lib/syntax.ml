open Sexplib.Std

open Tokens

(* notes on the language:
    - nodes are either expressions or statements
    - everything except let bindings are is expression but you can make it a statement by adding a semi-colon afterwards

    base syntax:
      - statements ....
        - let bindings (variable assignment):
            - let x : {type} = ....
            - let x : {type} = (... bunch of shit ...)
            - optionally: let x = ... (type is inferred)
        - let bindings (function declaration):
            - let func_name = fn (param: {type}, param: {type}, ...) : {type} { 
                // body (for now disalow function assignment)
              }
        - an expression with a semi-colon at the end (evaluates to unit)
      - expressions:
        - if statements (requires both branches) (type is inferred):
            - if cond { true_branch } else { false_branch }
        - function application (note takes higher precedence over operators)
          - f (...)
        - operator expressions (note precedence is as follows):
          - !, && , ||, (/, *.), (+, -)
*)


type type_annotation = 
  | Int
  | String
  | Bool
  | Unit
  | Function of (type_annotation list * type_annotation)
  | Custom of string
[@@deriving sexp]

type statement_t = unit [@@deriving sexp]
type expression_t = unit [@@deriving sexp]

(* represents the AST for a program *)
(* annotates AST blocks with their type *)
(* programs are simply a list of statements with an identified main function (entry point) *)
type 'a node_t = 
  { node: 'a node; mutable resolved_type: type_annotation option }

and expr_node = expression_t node_t
and statement_node = statement_t node_t

and _ node =
  | Statement : statement -> statement_t node
  | Expression : expression -> expression_t node

and expression =
  | IfBlock           of { condition: expr_node; if_block: expr_node; else_block: expr_node }
  | InfixApplication  of { operator: Tokens.operator; lhs: expr_node; rhs: expr_node }
  | BlockExpression   of { statements: (statement_node list); final_expr: expr_node }
  | PrefixApplication of { func_name: string; params: (expr_node list) }
  | Literal           of Tokens.literal
  | Variable          of string

and statement =
  | FunctionDeclaration of { name: string; output_type: type_annotation; params: (string * type_annotation) list; body: expr_node }
  | VariableDeclaration of { name: string; value: expr_node }
  | SilencedExpression  of expr_node


(* s-expression functions for serialization *)
(* these exist because for some reason s-expression error out when using GADTs (maybe i just cant use sexplib) *)
let rec property : type a. converter:(a -> Ppx_sexp_conv_lib.Sexp.t) -> string -> a -> Ppx_sexp_conv_lib.Sexp.t = 
  fun ~converter name value -> Base.Sexp.(List [Atom name; List [converter value]])
and n_property name value = property ~converter:sexp_of_node name value
and n_property_list name value = property name value ~converter:(sexp_of_list sexp_of_node)
and sexp_of_param (a, b) = Base.Sexp.(List [sexp_of_string a; sexp_of_node b])
and sexp_of_arg (a, b) = Base.Sexp.(List [sexp_of_string a; sexp_of_type_annotation b])

and sexp_of_expression = 
  let open Base.Sexp in function
    | IfBlock { condition; if_block; else_block } -> List [Atom "IfBlock"; List [n_property "condition" condition; n_property "if_block" if_block; n_property "else_block" else_block]]
    | InfixApplication { operator; lhs; rhs }     -> List [Atom "InfixApplication"; List [property "operator" operator ~converter:sexp_of_operator; n_property "lhs" lhs; n_property "rhs" rhs]]
    | BlockExpression { statements; final_expr }  -> List [Atom "BlockExpression"; List [n_property_list "statements" statements; n_property "final_expr" final_expr]]
    | PrefixApplication { func_name; params }     -> List [Atom "PrefixApplication"; List [ property ~converter:sexp_of_string "func_name" func_name; n_property_list "params" params]]
    | Literal literal -> List [Atom "Literal"; List [Atom "literal"; sexp_of_literal literal]]
    | Variable var    -> List [Atom "Variable"; List [Atom "var"; Atom var]]

and sexp_of_statement =
  let open Base.Sexp in function
    | SilencedExpression node -> List [Atom "SilencedExpression"; n_property "node" node]
    | VariableDeclaration { name; value } -> List [Atom "VariableDeclaration"; property ~converter:sexp_of_string "name" name; n_property "value" value]
    | FunctionDeclaration { name; output_type; params; body } ->
      List [
        Atom "FunctionDeclaration";
        List [Atom "name"; sexp_of_string name];
        List [Atom "output_type"; sexp_of_type_annotation output_type];
        List [Atom "params"; sexp_of_list sexp_of_arg params];
        List [Atom "body"; sexp_of_node body]
      ]

and sexp_of_node : type a. a node_t -> Ppx_sexp_conv_lib.Sexp.t = 
  let open Base.Sexp in function
    | { node = Statement s; resolved_type = t }  -> List [Atom "statement"; List [Atom "node"; sexp_of_statement s]; List [Atom "resolved_type"; sexp_of_option sexp_of_type_annotation t]]
    | { node = Expression e; resolved_type = t } -> List [Atom "expression"; List [Atom "node"; sexp_of_expression e]; List [Atom "resolved_type"; sexp_of_option sexp_of_type_annotation t]]


(* defined precedence for tokens, defined in sytanx since its a core part of syntax *)
let precedence = function
  | Add | Sub  -> 1
  | Mult | Div -> 2
  | Rem        -> 3
  | And        -> 4
  | Or         -> 5
  | Equality | Inequality | LessThanOrEq | GreaterThanOrEq | LessThan | GreaterThan -> 6
  | _ -> raise (Invalid_argument "precedences are only defined on operators")
