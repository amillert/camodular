open Ast
open Sem
module StringMap = Map.Make (String)

module Env = struct
  type 'a t = scopeT StringMap.t

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
        match mode with
        | NoInherit -> ()
        | Extend pid ->
            let parsed_scope =
              let joined_scopes = join_parent_scope pid vars in
              { internal = joined_scopes; exposed = joined_scopes }
            in
            env := StringMap.add id parsed_scope !env
        | Open pid ->
            let parsed_scope =
              let joined_scopes = join_parent_scope pid vars in
              { internal = joined_scopes; exposed = vars }
            in
            env := StringMap.add id parsed_scope !env)
end
