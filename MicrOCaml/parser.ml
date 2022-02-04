open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
match lookahead toks with
|Some Tok_Let -> parse_let toks
|Some Tok_Fun -> parse_fun toks
|Some Tok_If -> parse_if toks
|_ -> parse_or toks

and parse_let toks = 
let toks = match_token toks Tok_Let in
let is_rec = if lookahead toks = Some Tok_Rec then true else false in
let toks = if is_rec then match_token toks Tok_Rec else toks in
let this_ID = match lookahead toks with
|Some Tok_ID(b) -> b
|_ -> raise (InvalidInputException("parse_let"))
in let toks = match_many toks [Tok_ID(this_ID); Tok_Equal] in
match parse_expr toks with
|(rem_toks, expr1) -> let toks = match_token rem_toks Tok_In in
match parse_expr toks with
|(rem_toks, expr2) -> (rem_toks, Let(this_ID, is_rec, expr1, expr2))

and parse_fun toks =
let toks = match_token toks Tok_Fun in
let this_ID = match lookahead toks with 
|Some Tok_ID(b) -> b
|_ -> raise (InvalidInputException("parse_fun"))
in let toks = match_many toks [Tok_ID(this_ID); Tok_Arrow] in
match parse_expr toks with
|(rem_toks, expr) -> (rem_toks, Fun(this_ID, expr))

and parse_if toks =
let toks = match_token toks Tok_If in
match parse_expr toks with
|(rem_toks, expr1) -> let toks = match_token rem_toks Tok_Then in
match parse_expr toks with
|(rem_toks, expr2) -> let toks = match_token rem_toks Tok_Else in
match parse_expr toks with
|(rem_toks, expr3) -> (rem_toks, If(expr1, expr2, expr3))

and parse_or toks =
match parse_and toks with
|(rem_toks, expr1) -> if lookahead rem_toks = Some Tok_Or
then let toks = match_token rem_toks Tok_Or in
 match parse_or toks with
 |(rem_toks, expr2) -> (rem_toks, Binop(Or, expr1, expr2))
else (rem_toks, expr1)

and parse_and toks =
match parse_eq toks with
|(rem_toks, expr1) -> if lookahead rem_toks = Some Tok_And
then let toks = match_token rem_toks Tok_And in
 match parse_and toks with
 |(rem_toks, expr2) -> (rem_toks, Binop(And, expr1, expr2))
else (rem_toks, expr1)

and parse_eq toks =
match parse_rel toks with
|(rem_toks, expr1) -> if lookahead rem_toks = Some Tok_Equal then
let toks = match_token rem_toks Tok_Equal in
match parse_eq toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Equal, expr1, expr2))
else if lookahead rem_toks = Some Tok_NotEqual then
let toks = match_token rem_toks Tok_NotEqual in
match parse_eq toks with
|(rem_toks, expr2) -> (rem_toks, Binop(NotEqual, expr1, expr2))
else (rem_toks, expr1)

and parse_rel toks =
match parse_add toks with
|(rem_toks, expr1) -> if lookahead rem_toks = Some Tok_Greater then
let toks = match_token rem_toks Tok_Greater in
match parse_rel toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Greater, expr1, expr2))
else if lookahead rem_toks = Some Tok_Less then
let toks = match_token rem_toks Tok_Less in
match parse_rel toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Less, expr1, expr2))
else if lookahead rem_toks = Some Tok_GreaterEqual then
let toks = match_token rem_toks Tok_GreaterEqual in
match parse_rel toks with
|(rem_toks, expr2) -> (rem_toks, Binop(GreaterEqual, expr1, expr2))
else if lookahead rem_toks = Some Tok_LessEqual then
let toks = match_token rem_toks Tok_LessEqual in
match parse_rel toks with
|(rem_toks, expr2) -> (rem_toks, Binop(LessEqual, expr1, expr2))
else (rem_toks, expr1)

and parse_add toks =
match parse_mul toks with
|(rem_toks, expr1) -> if lookahead rem_toks = Some Tok_Add then
let toks = match_token rem_toks Tok_Add in
match parse_add toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Add, expr1, expr2))
else if lookahead rem_toks = Some Tok_Sub then
let toks = match_token rem_toks Tok_Sub in
match parse_add toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Sub, expr1, expr2))
else (rem_toks, expr1)

and parse_mul toks =
match parse_concat toks with
|(rem_toks, expr1) -> if lookahead rem_toks = Some Tok_Mult then
let toks = match_token rem_toks Tok_Mult in
match parse_mul toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Mult, expr1, expr2))
else if lookahead rem_toks = Some Tok_Div then
let toks = match_token rem_toks Tok_Div in
match parse_mul toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Div, expr1, expr2))
else (rem_toks, expr1)

and parse_concat toks = 
match parse_unary toks with
|(rem_toks, expr1) -> if lookahead rem_toks = Some Tok_Concat then
let toks = match_token rem_toks Tok_Concat in
match parse_concat toks with
|(rem_toks, expr2) -> (rem_toks, Binop(Concat, expr1, expr2))
else (rem_toks, expr1)

and parse_unary toks =
if lookahead toks = Some Tok_Not then
let toks = match_token toks Tok_Not in
match parse_unary toks with
|(rem_toks, expr) -> (rem_toks, Not(expr))
else parse_funcall toks

and parse_funcall toks =
match parse_prim toks with
|(rem_toks, expr1) ->  match lookahead rem_toks with
|Some Tok_Int(b) -> (match parse_prim rem_toks with (rem_toks, expr2) -> (rem_toks, FunctionCall(expr1, expr2)))
|Some Tok_Bool(b) -> (match parse_prim rem_toks with (rem_toks, expr2) -> (rem_toks, FunctionCall(expr1, expr2)))
|Some Tok_String(b) -> (match parse_prim rem_toks with (rem_toks, expr2) -> (rem_toks, FunctionCall(expr1, expr2)))
|Some Tok_ID(b) -> (match parse_prim rem_toks with (rem_toks, expr2) -> (rem_toks, FunctionCall(expr1, expr2)))
|Some Tok_LParen -> (match parse_prim rem_toks with (rem_toks, expr2) -> (rem_toks, FunctionCall(expr1, expr2)))
|_ -> (rem_toks, expr1)

and parse_prim toks =
match lookahead toks with
|Some Tok_Int(b) -> (match_token toks (Tok_Int(b)), Value(Int(b)))
|Some Tok_Bool(b) -> (match_token toks (Tok_Bool(b)), Value(Bool(b)))
|Some Tok_String(b) -> (match_token toks (Tok_String(b)), Value(String(b)))
|Some Tok_ID(b) -> (match_token toks (Tok_ID(b)), ID(b))
|Some Tok_LParen -> let toks = match_token toks Tok_LParen in
(match parse_expr toks with (rem_toks, expr) -> (match_token rem_toks Tok_RParen, expr))
|_ -> raise (InvalidInputException("parse_prim"))



(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
 if lookahead toks = Some Tok_DoubleSemi then
  ((match_token toks Tok_DoubleSemi), NoOp)
 else if lookahead toks = Some Tok_Def then
  let this_ID = match lookahead_many toks 1 with
  |Some Tok_ID(b) -> b
  |_ -> raise (InvalidInputException("parse DefMutop"))
  in let toks = match_many toks [Tok_Def; Tok_ID(this_ID); Tok_Equal]
  in match parse_expr toks with
  |(rem_toks, expr) -> let toks = match_token rem_toks Tok_DoubleSemi
  in (toks, Def(this_ID, expr))
 else match parse_expr toks with
  |(rem_toks, expr) -> let toks = match_token rem_toks Tok_DoubleSemi 
  in (toks, Expr(expr))
