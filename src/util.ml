let dedup a b = List.sort_uniq compare (a @ b)
let string_of_list = List.fold_left (fun acc x -> acc ^ " " ^ x) "\nPRINT:"

module Syntax = struct
  let ( *> ) f g =
    let () = f () in
    let () = print_newline () in
    g

  let ( @> ) f g =
    let () = f () in
    let () = print_newline () in
    g ()
end
