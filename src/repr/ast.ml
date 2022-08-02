type id = string
type inheritance_mode = Extend of id | Open of id | NoInherit

type ast =
  | ClassScope of { id : id; vars : string list; mode : inheritance_mode }
  | Empty
  | Invalid

let classNoVars ?(mode = NoInherit) id = ClassScope { id; vars = []; mode }
let classYesVars ?(mode = NoInherit) id vars = ClassScope { id; vars; mode }

let show : ast -> string = function
  | ClassScope { id; vars; _ } ->
      let part acc x = acc ^ " " ^ x in
      "ClassScope " ^ id ^ List.fold_left part "" vars
  | Empty -> "Empty"
  | Invalid -> "Invalid"

let print ast = ast |> show |> print_endline
