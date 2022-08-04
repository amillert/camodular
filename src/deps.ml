module StringMap = Map.Make (String)

module DependencyResolver = struct
  type 'a t = string StringMap.t

  let deps = ref StringMap.empty

  let class_names xs =
    let rec go acc = function
      | [] -> acc
      | "class" :: x :: tail -> go (x :: acc) tail
      | _ :: t -> go acc t
    in
    go [] xs

  let build dir =
    let class_pattern x = Filename.extension x |> ( = ) ".cls" in
    let class_files =
      Sys.readdir dir |> Array.to_list |> List.filter class_pattern
    in
    let whitespace_regex = Str.regexp "[ \n\r\x0c\t]+" in
    let get_classes_file filename =
      let ch = open_in (dir ^ "/" ^ filename) in
      let s = really_input_string ch (in_channel_length ch) in
      let () = close_in ch in
      let tokens = Str.split whitespace_regex s in
      (class_names tokens, filename)
    in
    let classes_file_pairs = List.map get_classes_file class_files in
    let f (cs, file) =
      let process classes filename =
        let mutate filename cl_name =
          deps := StringMap.add cl_name filename !deps
        in
        classes |> List.iter @@ mutate filename
      in
      process cs file
    in
    List.iter f classes_file_pairs
end
