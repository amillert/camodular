open Lexer
open Sedlexing
open Parser

let process filename () =
  let lexbuf = filename |> open_in |> Utf8.from_channel in
  let lx () = lex lexbuf () in
  let p = MenhirLib.Convert.Simplified.traditional2revised program in
  let ast = p lx in
  try match ast with _ -> print_endline "some pretty print"
  with _ ->
    let _ = failwith "BOOOM !" in
    exit (-1)

let () =
  let filename = "./files/classes.txt" in
  process filename ()
