
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


val show_expr : expr -> string

val expr1 : expr Parser.t
