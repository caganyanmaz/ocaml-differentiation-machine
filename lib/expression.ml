open Parser
open Core
open Utils

type fn = 
        | Neg
        | Abs
        | Exp
        | Log
        | Sin
        | Cos
        [@@deriving show]

type bin_op =
        | Pow
        | Mul
        | Div
        | Add
        | Sub
        [@@deriving show]

type expr =
        | Const       of int
        | Id
        | Bin_op      of bin_op * expr * expr
        | Unary_op    of fn * expr
        | Application of expr * expr
        | Diff        of expr
        [@@deriving show]

type statement =
        | Definition of string * expr
        | Value of expr
        [@@deriving show]

let keywords = []

let const =
        let* w = integer in
        return (Const w)

let id =
        let* w = symbol "x" in
        return Id

let addop = ops [
        (symbol "+", fun x y -> Bin_op (Add, x, y)); 
        (symbol "-", fun x y -> Bin_op (Sub, x, y))
]

let facop = ops [
        (symbol "*", fun x y -> Bin_op (Mul, x, y));
        (symbol "/", fun x y -> Bin_op (Div, x, y))
]

let powop = ops [symbol "^", fun x y -> Bin_op (Pow, x, y)]

let print_parser_states states = 
        let print_parser_state (x, y) =
                Printf.printf "(%s, %s)\n" (show_expr x) (list_to_string y) in (
        Printf.printf "----------------------------\n";
                List.iter states ~f:print_parser_state;
        Printf.printf "----------------------------\n"
        )
let _ = print_parser_states

let rec expr1 inp =
        let app = 
                let* left = expr2 in
                let* right = expr2 in
                return (Application (left, right)) in
        (app ++ expr2) inp
and expr2  inp = (chainl1 expr3 addop) inp
and expr3  inp = (chainl1 expr4 facop) inp
and expr4  inp = (chainr1 expr5 powop) inp
and expr5  inp = 
        let neg =
                let* _ = symbol "-" in
                let* e  = expr6 in
                return (Unary_op (Neg, e)) in
        let abs = 
                let* e = bracket (symbol "|") expr6 (symbol "|") in
                return (Unary_op (Abs, e)) in
        let exp = 
                let* _ = symbol "exp" in
                let* e = expr6 in
                return (Unary_op (Exp, e)) in
        let log = 
                let* _ = symbol "log" in
                let* e = expr6 in
                return (Unary_op (Log, e)) in
        let sin = 
                let* _ = symbol "sin" in
                let* e = expr6 in
                return (Unary_op (Sin, e)) in
        let cos = 
                let* _ = symbol "cos" in
                let* e = expr6 in
                return (Unary_op (Cos, e)) in
        let diff =
                let* e = expr6 in 
                let* _ = symbol "'" in
                return (Diff e) in
        (neg ++ abs ++ exp ++ log ++ sin ++ cos ++ diff ++ expr6) inp
and expr6 inp = 
        (id ++ const ++ bracket (symbol "(") expr1 (symbol ")")) inp


let parse_definition =
        let* name = identifier [] in
        let* _          = ch '=' in
        let* expr       = expr1 in
        return (Definition (list_to_string name, expr))

let parse_value =
        let* expr = expr1 in
        return (Value expr)

let parse_statement = parse_definition ++ parse_value


