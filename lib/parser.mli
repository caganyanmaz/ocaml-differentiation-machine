type 'a t = char list -> ('a * char list) list

val return : 'a -> 'a t

val zero : 'a t

val item : char t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val (>>|) : 'a t -> ('a -> 'b) -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val seq : 'a t -> 'b t -> ('a * 'b) t

val sat : (char -> bool) -> char t

val ch : char -> char t

val digit : char t

val upper : char t

val lower : char t

val (++) : 'a t -> 'a t -> 'a t

val letter : char t
val alphanum : char t

val word : (char list) t

val str : string -> char list t

val many : 'a t -> 'a list t

val many1 : 'a t -> 'a list t

val many1 : 'a t -> 'a list t

val ident : char list t

val nat : int t 

val int : int t

val sepby : 'a t -> 'b t -> 'a list t

val sepby1 : 'a t -> 'b t -> 'a list t

val bracket : 'a t -> 'b t -> 'c t -> 'b t

val chainl1 : 'a t -> (('a -> 'a -> 'a) t) -> 'a t

val chainr1 : 'a t -> (('a -> 'a -> 'a) t) -> 'a t

val ops : ('a t * 'b) list -> 'b t

val spaces : unit t

val junk : unit t

val parse : 'a t -> 'a t

val token : 'a t -> 'a t

val natural : int t

val integer : int t

val symbol : string -> char list t

val identifier : string list -> char list t

