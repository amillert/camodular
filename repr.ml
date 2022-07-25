(* Abstract Syntax Tree *)
(* Currently supported 2 modes of modular extension:
   1. Extend - freely exposes the scope,
   2. Open - encapsulates the scope making it invisible for a subclass. *)
type extension_mode = Extend | Open [@@deriving show { with_path = false }]

type repr =
  | ClassScope of { id : string; vars : string list; mode : extension_mode }
  | Empty
[@@deriving show { with_path = false }]

(* Allow extension by default for more flexibility. *)
let classNoVars ?(mode = Extend) id = ClassScope { id; vars = []; mode }
let classYesVars ?(mode = Extend) id vars = ClassScope { id; vars; mode }

(* Semantic representation *)
type scopeT = { internal : string list; exposed : string list }
[@@deriving show { with_path = false }]

let emptyScope = { internal = []; exposed = [] }

type classT = { id : string; scope : scopeT }
[@@deriving show { with_path = false }]

(* TODO: decouple into ast and sem modules + smart constructors *)
