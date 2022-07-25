(* -***- Abstract Syntax Tree -***- *)

type id = string [@@deriving show { with_path = false }]

type inheritance_mode = Extend of id | Open of id | NoInherit
[@@deriving show { with_path = false }]

type ast =
  | ClassScope of { id : id; vars : string list; mode : inheritance_mode }
  | Empty
  | Invalid
[@@deriving show { with_path = false }]

let classNoVars ?(mode = NoInherit) id = ClassScope { id; vars = []; mode }
let classYesVars ?(mode = NoInherit) id vars = ClassScope { id; vars; mode }
