open Repr.Ast
module StringMap = Map.Make (String)

module IdSet = Set.Make (struct
  type t = id

  let compare = compare
end)

module Env = struct
  open Repr.Ast
  open Repr.Scope

  type 'a t = scope StringMap.t

  let env = ref StringMap.empty

  let find_in_env id =
    match StringMap.find_opt id !env with None -> emptyScope | Some x -> x

  let add_to_env ~prints = function
    | Empty | Invalid ->
        if List.length prints = 0 then
          failwith "can't access if there's nothing"
        else ()
    | ClassScope { id; vars; mode } -> (
        let join_parent_scope pid vars =
          let parent_scope = find_in_env pid in
          Util.dedup parent_scope.exposed vars
        in
        let merge_scopes ?(expose = false) pid =
          let joined = join_parent_scope pid vars in
          let intersect =
            IdSet.inter (IdSet.of_list prints) (IdSet.of_list joined)
            |> IdSet.elements
          in
          if List.length intersect <> List.length prints then
            failwith "can't find all references in the scope"
          else if expose = true then
            if List.length intersect = List.length prints then
              { internal = joined; exposed = joined }
            else failwith "can't find all references in the scope"
          else { internal = joined; exposed = vars }
        in
        match mode with
        | NoInherit -> env := StringMap.add id (merge_scopes "?") !env
        | Extend pid ->
            env := StringMap.add id (merge_scopes pid ~expose:true) !env
        | Open pid -> env := StringMap.add id (merge_scopes pid) !env)
end
