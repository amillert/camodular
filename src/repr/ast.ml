type id = string [@@deriving show { with_path = false }]

type inheritance_mode = Extend of id | Open of id | NoInherit
[@@deriving show { with_path = false }]

type ast =
  | ClassScope of { id : id; vars : string list; mode : inheritance_mode }
  | Empty
  | Invalid
[@@deriving show { with_path = false }]

let show : ast -> string = function
  | ClassScope { id; vars; _ } ->
      let part acc x = acc ^ " " ^ x in
      "ClassScope " ^ id ^ List.fold_left part "" vars
  | Empty -> "Empty"
  | Invalid -> "Invalid"

let classNoVars ?(mode = NoInherit) id = ClassScope { id; vars = []; mode }
let classYesVars ?(mode = NoInherit) id vars = ClassScope { id; vars; mode }

let show : ast -> string = function
  | ClassScope { id; vars; _ } ->
      let part acc x = acc ^ " " ^ x in
      "ClassScope " ^ id ^ List.fold_left part "" vars
  | Empty -> "Empty"
  | Invalid -> "Invalid"

let print ast = ast |> show |> print_endline
