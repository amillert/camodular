module StringMap = Map.Make (String)

module DependencyResolver = struct
  type 'a t = string StringMap.t

  let deps = ref StringMap.empty

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
    (* let process (cs, f) = List.iter (fun c -> deps := StringMap.add c f !deps) cs in *)
end
