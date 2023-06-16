open Tokens
open Syntax
include ParserState

let (let+) = Option.bind

(* Core parsing logic for parsing the actual syntax of the monkey language itself *)
(* repeatedly consume statements until we cannot anymore *)
let rec parse_program ~parser =
  let rec inner parser acc = 
    match parse_statement ~parser with
    | Some (statement, parser) -> inner parser (acc @ [statement])
    | None -> if is_empty ~parser then Some (acc) else None 
  in
  inner parser []
  
(* parse_expression_prec parses a regular expression composed of binary operators 
     note: it factors in precedence *)
and parse_expression = parse_expression_prec ~precedence:1
and parse_expression_prec ~parser ~precedence =
  let parse_atom ~parser = 
    let current_token, parser = consume_current ~parser in
    let next_token, next_parser = consume_current ~parser in

    match (current_token, next_token) with
      | (Some (LParen), _)                       -> skip_parser_in_result ~result:(parse_expression ~parser)
      | (Some (Variable func_name), Some LParen) -> parse_invocation ~func_name ~parser:next_parser
      | (Some (Variable var_name), _)            -> parse_variable ~var_name ~parser
      | (Some (Literal lit), _)                  -> parse_literal ~parser ~lit
      | (Some (If), _)                           -> parse_if_statement ~parser
      | (Some (LCurly), _)                       -> parse_block_expression ~parser
      | _ -> None
  in
  
  let+ atom, parser = parse_atom ~parser in
  parse_binary_operators ~parser ~lhs:atom ~precedence

(* consume_if_token consumes from the parser if the current token is the provided token *)
and consume_if_token ~parser ~token = 
  let+ current_token = peek_current ~parser in
  if current_token = token then Some (advance_parser ~parser)
  else None

(* parse_binary_operators parses binary operators within an expression sequence
   binary operators are just stuff like additions and subtractions *)
and parse_binary_operators ~parser ~lhs ~precedence =
  match peek_current ~parser with
  | Some (Operator token) ->
    let token_precedence = Syntax.precedence token in
    if token_precedence >= precedence then
      begin
        let next_min_prec = token_precedence + 1 in
        let+ rhs, parser = parse_expression_prec ~parser:(advance_parser ~parser) ~precedence:next_min_prec in
        let operand = application_from_operator ~operator:token ~lhs ~rhs in

        parse_binary_operators ~parser ~lhs:operand ~precedence
      end
    else Some (lhs, parser)
  | _ -> Some (lhs, parser)


(* small helper for parsing expressions *)
and application_from_operator ~operator ~lhs ~rhs =
  {
    node = Expression (InfixApplication { operator; lhs; rhs });
    resolved_type = None
  }


(* parse_literal tries to parse the head of the parser as if it is a literal value *)
and parse_literal ~parser ~(lit: literal) =
  let construct_literal lit ~type_an = {
      node = Expression (Literal lit);
      resolved_type = Some (type_an);
    } 
  in
  match lit with
    | Int _ -> Some (construct_literal lit ~type_an:Int, parser)
    | Bool _ -> Some (construct_literal lit ~type_an:Bool, parser)
    | String _ -> Some (construct_literal lit ~type_an:String, parser)


(* parse_varialbe parses an expression in the form of a variable *)
and parse_variable ~var_name ~parser = Some ({ node = Expression (Variable var_name); resolved_type = None }, parser)


(* parse_invocation attempts to parse the head of a parsing stream as if its a function
  invocation *)
and parse_invocation ~func_name ~parser = 
  let rec parse_arguments ~parser = 
    let+ argument, parser = parse_expression ~parser in
    let current_token, parser = consume_current ~parser in
    match current_token with
      | Some RParen -> Some ([argument], parser)
      | Some Comma -> let+ other_arguments, parser = parse_arguments ~parser in Some ([argument] @ other_arguments, parser)
      | _ -> None
  in

  (* parse the arguments for the invocation and then create the invocation terminal *)
  let+ params, parser = parse_arguments ~parser in
  Some ({ node = Expression (PrefixApplication { func_name; params }); resolved_type = None; }, parser)

(* parse_if_statement attempts to parse an if block from the current parser stream *)
and parse_if_statement ~parser = 
  let+ condition, parser = parse_expression ~parser in
  let+ parser = consume_if_token ~parser ~token:Then in
  let+ if_block, parser = parse_expression ~parser in
  let+ else_block, parser = (
    match peek_current ~parser with
      | Some Else -> parse_expression ~parser:(advance_parser ~parser)
      | _ -> None
    ) in
  Some ({ node = Expression (IfBlock { condition; if_block; else_block }); resolved_type = None; }, parser)
 

and parse_block_expression ~parser =
  let rec inner ~parser ~acc = 
    match parse_statement ~parser with
      | Some (statement, parser) -> inner ~parser ~acc:([statement] @ acc)
      | None -> (acc, parser)    
  in
  
  let statements, parser = inner ~parser ~acc:[] in
  let+ final_expr, parser = parse_expression ~parser in
  let+ parser = consume_if_token ~parser ~token:RCurly in

  Some ({ node = Expression (BlockExpression { statements; final_expr }); resolved_type = None; }, parser)










(* statement parsing logic *)
and parse_statement ~parser =
  let old_parser = parser in
  let current_token, parser = consume_current ~parser in
  let next_token, next_parser = consume_current ~parser in

  match (current_token, next_token) with
    | (Some (Let), Some (Variable (name))) -> parse_declaration ~parser:next_parser ~name
    | _ -> 
      (* if we can't match an assignment then we need to match a silenced expression *)
      let+ expression, parser = parse_expression ~parser:old_parser in
      let+ parser = consume_if_token ~parser ~token:Semicolon in
      Some ({ node = Statement (SilencedExpression expression); resolved_type = Some (Unit)}, parser)

(* declarations can either be function declarations or variable declarations
   this function just dispatches it to the appropriate one *)
and parse_declaration ~name ~parser = 
  match (peek_current ~parser, peek_next ~parser) with
    | (Some (Assignment), Some (Fn)) -> parse_function_declaration ~name ~parser:(skip ~n:2 ~parser)
    | (Some (Assignment), _)         -> parse_variable_declaration ~name ~parser:(advance_parser ~parser)
    | _ -> None

(* helpers for function argument parsing *)
(* parse_func_param parses an individual param within a function *)
and parse_func_param ~parser =
  let arg_name, parser = consume_current ~parser in
    let+ parser = consume_if_token ~parser ~token:Colon in
    let type_annotation, parser = consume_current ~parser in

    match (arg_name, type_annotation) with
      | (Some (Variable name), Some (Variable type_str)) -> Some ((name, parse_type type_str), parser)
      | _ -> None
  
(* parse_func_params parses all the parameters to a function *)
and parse_func_params ~parser = 
  let+ (arg_name, arg_type), parser = parse_func_param ~parser in
  let current_token, parser = consume_current ~parser in
  match current_token with
    | Some RParen -> Some ([(arg_name, arg_type)], parser)
    | Some Comma -> let+ other_arguments, parser = parse_func_params ~parser in Some ([(arg_name, arg_type)] @ other_arguments, parser)
    | _ -> None

and create_func_decl ~parser name params body type_str = 
  Some ({
    node = Statement (FunctionDeclaration {name; params; body; output_type = (parse_type type_str) }); 
    resolved_type = None 
  }, parser)

and parse_function_declaration ~name ~parser =
  let+ params, parser = parse_func_params ~parser:(advance_parser ~parser) in
  let out_type, parser = consume_current ~parser in
  let next_token, parser = consume_current ~parser in

  (* match on the function definitions now *)
  match (out_type, next_token) with
    | (Some (Variable type_str), Some (Arrow)) -> let+ body, parser = parse_expression ~parser in  create_func_decl ~parser name params body type_str
    | _ -> None


and parse_variable_declaration ~parser ~name =
  (* determine if this declaration is type annotated *)
  let resolved_type, parser = 
    match (peek_current ~parser, peek_next ~parser) with
    | (Some (Colon), Some (Variable x)) -> (Some (parse_type x), skip ~n:2 ~parser)
    | _ -> (None, parser) in

  let+ parser = consume_if_token ~parser ~token:Assignment in
  let+ value, parser = parse_expression ~parser in
  Some ({ node = Statement (VariableDeclaration { name; value }); resolved_type }, parser)


and parse_type: string -> type_annotation = function
  | "string" -> String
  | "int"    -> Int
  | "bool"   -> Bool
  | "unit"   -> Unit
  | type_str -> Custom type_str




  







(* ==== tests ==== *)
let%test "parses_invocation_expr" = 
  let example_program = {|
    let fact = fn (n: int) int => { 
      if n == 0 then 1 
      else 
        n * fact (n - 1)
    }

    let main = fn (argv: int) int => {
      printInt(fact(3));
      0
    }
|} in
  let lexer = Lexer.new_lexer ~stream:example_program in
  let parser = new_parser ~lexer in

  match parse_program ~parser with
    | None -> false
    | Some program ->
      let sexp = Base.Sexp.to_string (Base.sexp_of_list sexp_of_node program) in
      print_string sexp;
      print_newline ();
      true