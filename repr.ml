(* Abstract Syntax Tree *)
type repr = ClassScope of { id : string; vars : string list } | Empty
[@@deriving show { with_path = false }]

(* Semantic representation *)
type scopeT = { internal : string list; exposed : string list }
[@@deriving show { with_path = false }]

type classT = { id : string; scope : scopeT }
[@@deriving show { with_path = false }]
