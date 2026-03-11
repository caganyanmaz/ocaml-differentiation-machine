let (<<) f g x = f (g x)
let list_to_string = String.of_seq << List.to_seq
let string_to_list = List.of_seq << String.to_seq
