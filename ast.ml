(* -***- Abstract Syntax Tree -***- *)

(* Currently supported 2 modes of modular extension:
   1. Extend - freely exposes the scope,
   2. Open - encapsulates the scope making it invisible for a subclass. *)
type extension_mode = Extend | Open [@@deriving show { with_path = false }]

type repr =
  | ClassScope of { id : string; vars : string list; mode : extension_mode }
  | Empty
  | Invalid
[@@deriving show { with_path = false }]

(* Allow extension by default for more flexibility. *)
let classNoVars ?(mode = Extend) id = ClassScope { id; vars = []; mode }
let classYesVars ?(mode = Extend) id vars = ClassScope { id; vars; mode }
