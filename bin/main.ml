open Ocaml_differentiation_machine
open Diffnum

let rec iterate (f: 'a -> 'a) (n: int) (v: 'a) : 'a =
        if n = 0 then v
        else          iterate f (n-1) (f v)

let () =
        while true do
                let line = read_line () in
                Ocaml_differentiation_machine.Eval.eval_line line
        done
