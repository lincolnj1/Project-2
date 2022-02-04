open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =

let re_pos_int = Str.regexp "[0-9]+" in
let re_neg_int = Str.regexp "(-[0-9]+)" in
let re_add = Str.regexp "+" in
let re_bool = Str.regexp "true\\|false" in
let re_lparen = Str.regexp "(" in
let re_rparen = Str.regexp ")" in
let re_equal = Str.regexp "=" in
let re_str = Str.regexp "\"[^\"]*\"" in
let re_ID = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
let re_nequal = Str.regexp "<>" in
let re_greater = Str.regexp ">" in
let re_less = Str.regexp "<" in
let re_gr_eq = Str.regexp ">=" in
let re_le_eq = Str.regexp "<=" in
let re_or = Str.regexp "||" in
let re_and = Str.regexp "&&" in
let re_not = Str.regexp "not" in
let re_if = Str.regexp "if" in
let re_then = Str.regexp "then" in
let re_else = Str.regexp "else" in
let re_sub = Str.regexp "-" in
let re_mult = Str.regexp "*" in
let re_div = Str.regexp "/" in
let re_concat = Str.regexp "\\^" in
let re_let = Str.regexp "let" in
let re_def = Str.regexp "def" in
let re_in = Str.regexp "in" in
let re_rec = Str.regexp "rec" in
let re_fun = Str.regexp "fun" in
let re_arrow = Str.regexp "->" in
let re_doublesemi = Str.regexp ";;" in

let rec strip_whitespaces s pos =
if pos < String.length s then
if Str.string_match (Str.regexp " \\|\n\\|\t") s pos then strip_whitespaces s (pos + 1) else pos
else pos

in let rec check_reg regexp s pos max_len =
if Str.string_match regexp s pos && String.length (Str.matched_string s) > max_len
then true else false

in let rec iter_regexps regexps s pos largest_len token =
match regexps with
|[] -> (token, largest_len)
|(a,b)::t -> if check_reg a s pos largest_len
then iter_regexps t s pos (String.length (Str.matched_string s)) (Some b)
else iter_regexps t s pos largest_len token

in let rec tok s pos tokens =

 let pos = strip_whitespaces s pos 
 in if pos >= String.length s then tokens else

  let regexps =
  [(re_add, Tok_Add); (re_lparen, Tok_LParen); (re_rparen, Tok_RParen); (re_equal, Tok_Equal); (re_nequal, Tok_NotEqual);
  (re_greater, Tok_Greater); (re_less, Tok_Less); (re_gr_eq, Tok_GreaterEqual); (re_le_eq, Tok_LessEqual);
  (re_or, Tok_Or); (re_and, Tok_And); (re_not, Tok_Not); (re_if, Tok_If); (re_then, Tok_Then); (re_else, Tok_Else);
  (re_sub, Tok_Sub); (re_mult, Tok_Mult); (re_div, Tok_Div); (re_concat, Tok_Concat); (re_let, Tok_Let); (re_def, Tok_Def);
  (re_in, Tok_In); (re_rec, Tok_Rec); (re_fun, Tok_Fun); (re_arrow, Tok_Arrow); (re_doublesemi, Tok_DoubleSemi)]

  in let new_tok = iter_regexps regexps s pos 0 None

  in let new_tok = match new_tok with
  |(_, largest_len) -> if check_reg re_pos_int s pos largest_len 
  then (Some (Tok_Int(int_of_string (Str.matched_string s))), String.length (Str.matched_string s)) else new_tok

  in let new_tok = match new_tok with
  |(_, largest_len) -> if check_reg re_neg_int s pos largest_len
  then (Some (Tok_Int(int_of_string (String.sub (Str.matched_string s) 1 ((String.length (Str.matched_string s)) - 2)))), String.length (Str.matched_string s)) else new_tok
 
  in let new_tok = match new_tok with
  |(_, largest_len) -> if check_reg re_bool s pos largest_len
  then (Some (Tok_Bool(bool_of_string (Str.matched_string s))), String.length (Str.matched_string s)) else new_tok

  in let new_tok = match new_tok with
  |(_, largest_len) -> if check_reg re_str s pos largest_len
then (Some (Tok_String(String.sub (Str.matched_string s) 1 ((String.length(Str.matched_string s)) - 2))),
		      String.length (Str.matched_string s)) else new_tok

  in let new_tok = match new_tok with
  |(_, largest_len) -> if check_reg re_ID s pos largest_len
  then (Some (Tok_ID(Str.matched_string s)), String.length (Str.matched_string s)) else new_tok

  in match new_tok with 
  |(Some a, b) -> tok s (pos+b) (a::tokens)
  |(None, _) -> raise (Not_found)

in let tok_list = tok input 0 []

in let rec rev_lst lst acc =
 match lst with
  |[] -> acc
  |x::t -> rev_lst t (x::acc)

in rev_lst tok_list []
