module StringMap = Map.Make (String)

module DependencyResolver = struct
  type 'a t = string StringMap.t

  let deps = ref StringMap.empty

  exception DepsException of string

  let find_in_deps x =
    match StringMap.find_opt x !deps with
    | None -> raise @@ DepsException "Can't find file in dir"
    | Some x -> x

  let build dir =
    let get_classes_file filename =
      let whitespace_regex = Str.regexp "[ \n\r\x0c\t]+" in
      let rec extract_class_names ?(acc = []) = function
        | [] -> acc
        | "class" :: x :: tail -> extract_class_names ~acc:(x :: acc) tail
        | _ :: t -> extract_class_names ~acc t
      in
      let ch = open_in @@ dir ^ "/" ^ filename in
      let s = in_channel_length ch |> really_input_string @@ ch in
      let () = close_in ch in
      let tokens = Str.split whitespace_regex s in
      (extract_class_names tokens, filename)
    in
    let file_names =
      let class_extension_pattern x = Filename.extension x |> ( = ) ".cls" in
      Sys.readdir dir |> Array.to_list |> List.filter class_extension_pattern
    in
    let classes_file_pairs = List.map get_classes_file file_names in
    let process (cs, f) =
      let mutate c = deps := StringMap.add c f !deps in
      List.iter mutate cs
    in
    List.iter process classes_file_pairs

  (* let run _class_name = *)
  (*   let process () = *)
  (*     let open Repr.Ast in *)
  (*     let open Env in *)
  (*     let filename_opt = StringMap.find_opt _class_name !deps in *)
  (*     match filename_opt with *)
  (*     (* Even the class name / file are not found in directory - fail. *) *)
  (*     | None -> false *)
  (*     | Some filename -> *)
  (*         (* Find the extended class if any and recursively keep adding to env. *) *)
  (*         let lexbuf = filename |> open_in |> Sedlexing.Utf8.from_channel in *)
  (*         let parse = *)
  (*           MenhirLib.Convert.Simplified.traditional2revised Parser.program *)
  (*         in *)
  (*         let asts = *)
  (*           try lexbuf |> Lexer.lex |> parse *)
  (*           with _ -> failwith "Unexpected error; can't recover - BOOOM !" *)
  (*         in *)
  (*         (* let xd = function *) *)
  (*         (*   | ClassScope { id ; vars ; mode } -> match mode with *) *)
  (*         (*   | Extend id | Open id | NoInherit *) *)
  (*         (* | Empty ->  *) *)
  (*         (* | Invalid ->  *) *)
  (*         (* let _ = List.iter xd asts in *) *)
  (*         true *)
  (*   in *)
  (*   process () *)

  let show () =
    let pp fmt = StringMap.iter @@ Format.fprintf fmt "%s -> %s@\n" in
    Format.printf "%a" pp !deps
end
