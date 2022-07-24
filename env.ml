open Repr
module StringMap = Map.Make (String)

module Env = struct
  type 'a t = scopeT StringMap.t

  let env = ref StringMap.empty

  let findInEnv id =
    match StringMap.find_opt id !env with None -> emptyScope | Some x -> x

  let mutateEnv cls =
    let update ~oldScope:{ internal; exposed } =
      let newInternal = Util.dedup internal cls.scope.internal in
      let newExposed = Util.dedup exposed cls.scope.exposed in
      { internal = newInternal; exposed = newExposed }
    in
    match findInEnv cls.id with
    | { internal = []; exposed = [] } ->
        env := StringMap.add cls.id cls.scope !env
    (* TODO: Check visibility of vars for this class, etc. *)
    | s -> env := StringMap.add cls.id (update ~oldScope:s) !env
end
