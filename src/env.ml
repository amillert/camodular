module StringMap = Map.Make (String)

module Env = struct
  open Repr.Ast
  open Repr.Scope

  type 'a t = scope StringMap.t

  let env = ref StringMap.empty

  let find_in_env id =
    match StringMap.find_opt id !env with None -> emptyScope | Some x -> x

  let add_to_env = function
    | Empty | Invalid -> ()
    | ClassScope { id; vars; mode } -> (
        let join_parent_scope pid vars =
          let parent_scope = find_in_env pid in
          Util.dedup parent_scope.exposed vars
        in
        let parse_scopes ?(expose = false) pid =
          let joined = join_parent_scope pid vars in
          if expose = true then { internal = joined; exposed = joined }
          else { internal = joined; exposed = vars }
        in
        match mode with
        | NoInherit -> env := StringMap.add id (parse_scopes "?") !env
        | Extend pid ->
            env := StringMap.add id (parse_scopes pid ~expose:true) !env
        | Open pid -> env := StringMap.add id (parse_scopes pid) !env)
end
