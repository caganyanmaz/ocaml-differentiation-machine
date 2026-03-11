open Core
open Utils


type 'a t = char list -> ('a * char list) list

let return v inp = [(v, inp)]

let zero inp = []

let item = function
        | [] -> []
        | x :: xs -> [(x, xs)]


let bind p f inp = List.concat_map ~f:(fun(a, inp) -> f a inp) (p inp)

let map f p = bind p (fun x -> return (f x))

let (>>=) p f = bind p f

let (>>|) p f = map f p

let (let*) x f = bind x f

let seq p q = 
        p >>= fun x -> 
        q >>= fun y -> 
        return (x, y)


let sat p = 
        item
        >>= (fun x -> if p x then return x else zero)

let ch c = sat (fun x -> Char.equal x c)

let digit = sat Char.is_digit
let upper = sat Char.is_uppercase
let lower = sat Char.is_lowercase

let (++) p q inp = List.concat [p inp; q inp]

let letter = upper ++ lower
let alphanum = letter ++ digit

let rec word input = 
        let ne_word = 
                letter >>= fun x ->
                word >>= fun xs ->
                return (x :: xs) in
        (ne_word ++ return []) input

let str text = 
        let rec aux = function
                | []        -> return []
                | (x :: xs) -> 
                                ch x >>= fun _ ->
                                aux xs >>= fun _ ->
                                return (x :: xs) in
        aux (string_to_list text)

let rec many p =
        let inner = 
                let* x   = p in
                let* xs = many p  in
                return (x :: xs) in 
        inner ++ return []

let rec many1 p = 
        let* x = p in
        let* xs = many p in
        return (x :: xs)

let ident = 
        let* x = lower in
        let* xs = many alphanum in
        return (x :: xs)

let nat =
        let* xs = many1 digit in
        let res = (Int.of_string << list_to_string) xs in
        return res

let longest_ones f inp = 
        let results = f inp in
        let shortest_leftover = 
                List.min_elt ~compare:(fun (a, b) (c, d) -> List.(length b - length d)) results in
        match shortest_leftover with
        | None -> []
        | Some (a, b) ->
                let shortest_string_length = List.length b in
                List.filter results ~f:(fun (a, b) -> List.length b = shortest_string_length)

let int =
        let neg = 
                let* _   = ch '-' in
                let* num = nat in
                return (-num) in
        longest_ones (neg ++ nat)

let rec sepby1 p q =
        let* x  = p in
        let* xs = sepby p q in
        return (x :: xs)
and sepby p q = sepby1 p q ++ return []

let bracket left p right =
        let* _ = left in
        let* x = p in
        let* _ = right in
        return x


let chainl1 (p: 'a t) (op: ('a -> 'a -> 'a) t) =
        let* x: 'a  = p in
        let* op_pairs: (('a -> 'a -> 'a) * 'a) list  = many (seq op p) in
        let res = List.fold_left op_pairs ~init:x ~f:(fun a (f, b) -> f a b) in
        return res

let chainr1 p op = 
        let* op_pairs = many (seq op p) in
        let* x = p in
        let res = List.fold_right op_pairs ~f:(fun (f, a) b -> f a b) ~init:x in
        return res


let ops xs inp =
        let comb (p, op) =
                let* _ = p in
                return op in
        List.concat_map xs ~f:(fun pair -> comb pair inp)


let spaces = 
        let is_space c = (Char.equal c ' ') || (Char.equal c '\t') in
        let* _ = many1 (sat is_space) in
        return ()

let junk =
        let* _ = many spaces in
        return ()

let parse p =
        let* _ = junk in
        p

let token p = 
        let* v = p in
        let* _ = junk in
        return v

let natural = token nat

let integer = token int

let symbol = token << str

let identifier xs =
        let* x = ident in
        if List.for_all xs ~f:(fun w -> not (String.equal (list_to_string x) w)) 
                then return x
                else zero

