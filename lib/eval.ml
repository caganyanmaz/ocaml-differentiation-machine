open Expression
open Utils
open Core
open Option
open Diffnum

let (let*) = (>>=)

let apply_bin_op op =
        match op with 
        | Pow -> My_diff_float.pow
        | Mul -> My_diff_float.mul
        | Div -> My_diff_float.div
        | Add -> My_diff_float.add
        | Sub -> My_diff_float.sub

let apply_unary_op op = 
        match op with
        | Neg -> My_diff_float.neg
        | Abs -> My_diff_float.abs
        | Exp -> My_diff_float.exp
        | Log -> My_diff_float.log
        | Sin -> My_diff_float.sin
        | Cos -> My_diff_float.cos


let rec eval_with v = function
        | Id      -> My_diff_float.id (My_diff_float.eval v)
        | Const x -> My_diff_float.const (Float.of_int x)
        | Bin_op (op, a, b) ->
                let lres = eval_with v a in
                let rres = eval_with v b in
                apply_bin_op op lres rres
        | Unary_op (op, a) ->
                let res = eval_with v a in
                apply_unary_op op res
        | Application (a, b) ->
                let rres = eval_with v b in
                eval_with rres a
        | Diff a ->
                let res = eval_with v a in
                My_diff_float.diff res

and try_eval = function
        | Id -> None
        | Const x -> return (My_diff_float.const (Float.of_int x))
        | Bin_op (op, a, b) ->
                let* lres = try_eval a in
                let* rres = try_eval b in
                return (apply_bin_op op lres rres)
        | Unary_op (op, a) ->
                let* res = try_eval a in
                return (apply_unary_op op res)
        | Application (a, b) ->
                let* rres = try_eval b in
                return (eval_with rres a)
        | Diff a ->
                let* res = try_eval a in
                return (My_diff_float.diff res)


let eval e =
        match try_eval e with
        | None     -> Printf.printf "No free variables allowed!\n"
        | (Some v) -> Printf.printf "%f\n" (My_diff_float.eval v)

let eval_line line =
        let outputs = expr1 (string_to_list line) in
        let results = List.filter outputs ~f:(fun (e, r) -> List.is_empty r) in
        match results with
                | [] -> Printf.printf "No valid parse\n"
                | (x, _) :: (y, _) :: xs -> Printf.printf "Ambiguous statement: %s %s\n" (show_expr x) (show_expr y)
                | [(x, _)] -> eval x
