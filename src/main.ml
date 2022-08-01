open Sedlexing

let process filename () =
  let lexbuf = filename |> open_in |> Utf8.from_channel in
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let asts = lexbuf |> Lexer.lex |> parse in
  List.iter Repr.Ast.print asts

let () =
  let open Util.Syntax in
  let filenameA = "./files/testa" in
  let filenameB = "./files/testb" in
  process filenameA *> process filenameB @> process filenameA
