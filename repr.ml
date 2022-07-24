(* Abstract Syntax Tree *)
type repr = ClassScope of { id : string; vars : string list } | Empty
[@@deriving show { with_path = false }]

let classNoVars id = ClassScope { id; vars = [] }
let classYesVars id vars = ClassScope { id; vars }

(* Semantic representation *)
type scopeT = { internal : string list; exposed : string list }
[@@deriving show { with_path = false }]

let emptyScope = { internal = []; exposed = [] }

type classT = { id : string; scope : scopeT }
[@@deriving show { with_path = false }]
