module type Num_sig = sig
        type t
        val zero : t
        val one : t
        val minus_one : t

        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val neg : t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val abs : t -> t
end

module type Diff_num_sig = sig
        include Num_sig
        type nt
        val const : nt -> t
        val id : nt -> t
        val zip_with : (nt -> nt -> nt) -> t -> t -> t
        val map : (nt -> nt) -> t -> t
        val diff : t      -> t
        val cons : nt * t Lazy.t -> t
        val uncons : t -> nt * t Lazy.t
        val eval : t -> nt
end


module Diff_num (N : Num_sig) : Diff_num_sig with type nt := N.t  = struct
        type t  = D of N.t * t Lazy.t
        type nt = N.t

        let rec zero  = D (N.zero, lazy zero)
        let one       = D (N.one, lazy zero)
        let minus_one = D (N.minus_one, lazy zero)

        let const x  = D (x, lazy zero)
        let id x     = D (x, lazy one)

        let rec zip_with f xs ys =
                match (xs, ys) with
                | (D (x, xs), D (y, ys)) -> D (f x y, lazy (zip_with f (Lazy.force xs) (Lazy.force ys)))
        let rec map f = function
                | D (x, xs) -> D (f x, lazy (map f (Lazy.force xs)))

        let add = zip_with N.add
        let sub = zip_with N.sub
        let rec mul f g =
                match (f, g) with
                | (D (a, f'), D (b, g')) -> D (N.mul a b, lazy (add (mul f (Lazy.force g')) (mul g (Lazy.force f'))))
        let neg = map N.neg

        let compare f g = 
                match (f, g) with
                | (D (a, f'), D (b, g')) -> N.compare a b

        let equal f g = (compare f g) = 0

        let abs f =
                let signum x =
                        let res = N.compare x N.zero in
                        if res = 0 then zero else (if res < 0 then minus_one else one) in
                match f with
                | D (a, f') -> 
                        let sign = signum a in
                        if (equal zero sign) then raise (Invalid_argument "hi")
                        else D (N.abs a, lazy (mul sign (Lazy.force f')))

        let diff = function
                | D (_, f') -> Lazy.force f'

        let cons (a, f') = D (a, f')

        let uncons = function D (a, f') -> (a, f')

        let eval = function D (a, f') -> a

end

module type Num_to_float_sig = sig
        type t
        val div : t -> t -> t
        val exp : t -> t
        val log : t -> t
        val pow : t -> t -> t
        val sin : t -> t
        val cos : t -> t
        val tan : t -> t
end

module type Float_sig = sig
        include Num_sig
        include Num_to_float_sig with type t := t
end

module type Diff_float_sig = sig
        include Diff_num_sig
        include Num_to_float_sig with type t := t
end

module Diff_float  (Float : Float_sig) : Diff_float_sig with type nt := Float.t = struct
        include Diff_num (Float)
        let rec div f g =
                let (a, f') = uncons f in
                let (b, g') = uncons g in
                let value     = Float.div a b in
                let numerator = lazy (sub (mul (Lazy.force f') g)  (mul f (Lazy.force g')))
                in
                let denominator = lazy (mul g g) in
                let diff = lazy (div (Lazy.force numerator) (Lazy.force denominator)) in
                cons (value, diff)
        let rec exp f =
                let (a, f') = uncons f in
                cons (Float.exp a, lazy (mul (exp f) (Lazy.force f')))
        let log f =
                let (a, f') = uncons f in 
                let value   = Float.log a in
                let diff    = lazy (div (Lazy.force f') f) in
                cons (value, diff)

        let rec pow f g = exp (mul (log f) g)

        let rec sin f =
                let (a, f') = uncons f in
                let value   = Float.sin a in
                let diff    = lazy (mul (cos f) (Lazy.force f')) in
                cons (value, diff)
            and cos f =
                let (a, f') = uncons f in
                let value   = Float.cos a in
                let diff    = lazy (mul (neg (sin f)) (Lazy.force f')) in
                cons (value, diff)

        let tan f = div (sin f) (cos f)
end 

module My_int : Num_sig = struct include Int end

module My_float : Float_sig = struct include Float end

module My_diff_int : Diff_num_sig with type nt := Int.t = Diff_num (Int)

module My_diff_float : Diff_float_sig with type nt := Float.t = Diff_float (Float)

