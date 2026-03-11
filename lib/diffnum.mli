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


module Diff_num (N : Num_sig) : Diff_num_sig with type nt := N.t

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

module Diff_float  (Float : Float_sig) : Diff_float_sig with type nt := Float.t 

module My_int : Num_sig 

module My_float : Float_sig 

module My_diff_int : Diff_num_sig with type nt := Int.t 

module My_diff_float : Diff_float_sig with type nt := Float.t 

