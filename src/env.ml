open Repr.Ast
open Util
module DR = Deps.DependencyResolver
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

  exception EnvException of string

  let find_in_env id =
    match StringMap.find_opt id !env with
    | None -> raise @@ EnvException "Can't find extended module in env"
    | Some x -> x

  let process filename () =
    let open Repr.Ast in
    let lexbuf = filename |> open_in |> Sedlexing.Utf8.from_channel in
    let expect_failure =
      1
      |> List.nth @@ String.split_on_char '.' filename
      |> String.split_on_char '_' |> List.rev |> List.hd = "fail"
    in
    let parse =
      MenhirLib.Convert.Simplified.traditional2revised Parser.program
    in
    let asts =
      try lexbuf |> Lexer.lex |> parse with
      | EnvException msg when expect_failure ->
          let () = print_endline @@ "Graceful recovery from EnvError: " ^ msg in
          [ Invalid ]
      | EnvException msg ->
          let () = print_endline @@ "Dirty recovery from EnvError: " ^ msg in
          failwith msg
      | _ -> failwith "Unexpected error; can't recover - BOOOM !"
    in
    List.iter print asts

  let add_to_env ~prints = function
    | Empty | Invalid -> ()
    | ClassScope { id; vars; mode } -> (
        let join_parent_scope pid =
          let parent_scope =
            let filename =
              try DR.find_in_deps pid with
              | DR.DepsException msg ->
                  let () = print_endline "Class not found as dependency" in
                  failwith msg
              | _ -> failwith "xd ?"
            in
            (* recursively parse file to resolve deps backwards. *)
            let () = process filename () in
            find_in_env pid
          in
          Util.dedup parent_scope.exposed vars
        in
        let merge_scopes ?(expose = false) pid =
          let joined = join_parent_scope pid in
          let print_uniq = IdSet.of_list prints in
          let joined_uniq = IdSet.of_list joined in
          let missing_list =
            IdSet.diff print_uniq joined_uniq |> IdSet.elements
          in
          if List.length missing_list <> 0 then
            let mis_str = List.fold_left ( ^ ) "" missing_list in
            raise @@ EnvException ("Missing references in the scope: " ^ mis_str)
            (* ^ "\njoined was: " ^ List.fold_left ( ^ ) "" joined) *)
          else if expose = true then { internal = joined; exposed = joined }
          else { internal = joined; exposed = vars }
        in
        let () =
          if List.length prints <> 0 then
            prints |> string_of_list |> print_endline
          else ()
        in
        match mode with
        | NoInherit ->
            let internal_scope = { internal = vars; exposed = vars } in
            env := StringMap.add id internal_scope !env
        | Extend pid ->
            (* resolve_backward pid based on deps *)
            env := StringMap.add id (merge_scopes pid ~expose:true) !env
        | Open pid -> env := StringMap.add id (merge_scopes pid) !env)
end
