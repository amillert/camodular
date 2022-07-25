(* -***- Semantic representation -***- *)
type scopeT = { internal : string list; exposed : string list }
[@@deriving show { with_path = false }]

let emptyScope = { internal = []; exposed = [] }

type classT = { id : string; scope : scopeT }
[@@deriving show { with_path = false }]
