let dedup a b = List.sort_uniq compare (a @ b)

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
