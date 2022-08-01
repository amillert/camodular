let process filename () =
  let open Repr.Ast in
  let open Env in
  let lexbuf = filename |> open_in |> Sedlexing.Utf8.from_channel in
  let expectFailure =
    filename |> String.split_on_char '_' |> List.rev |> List.hd = "fail"
  in
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let asts =
    try lexbuf |> Lexer.lex |> parse with
    | Env.EnvException msg when expectFailure ->
        let () = print_endline @@ "Graceful recovery from EnvError: " ^ msg in
        [ Invalid ]
    | Env.EnvException msg ->
        let () = print_endline @@ "Dirty recovery from EnvError: " ^ msg in
        failwith msg
    | _ -> failwith "Unexpected error; can't recover - BOOOM !"
  in
  List.iter print asts

let () =
  let open Util.Syntax in
  let filename1 = "./files/testa_ok" in
  let filename2 = "./files/testb_ok" in
  let filename3 = "./files/testc_fail" in
  process filename1 *> process filename3 @> process filename2
