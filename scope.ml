type scope = { internal : string list; exposed : string list }
[@@deriving show { with_path = false }]

let emptyScope = { internal = []; exposed = [] }
