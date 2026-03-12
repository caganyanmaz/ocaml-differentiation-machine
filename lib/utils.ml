open Core
let (<<) f g x = f (g x)
let list_to_string = String.of_char_list
let string_to_list = String.to_list
