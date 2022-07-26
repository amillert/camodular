open Parser
open Sedlexing

exception LexError of Lexing.position * string

let lower_alpha = [%sedlex.regexp? 'a' .. 'z']
let upper_alpha = [%sedlex.regexp? 'A' .. 'Z']
let alpha = [%sedlex.regexp? Plus (lower_alpha | upper_alpha)]
let var_identifier = [%sedlex.regexp? lower_alpha, Star (lower_alpha | '_')]
let class_identifier = [%sedlex.regexp? upper_alpha, Star alpha]

(* recursively apply regex to attempt constructing AST *)
let rec token buf =
  match%sedlex buf with
  | eof -> EOF
  | white_space -> token buf
  | "class" -> CLASS
  | class_identifier -> ID (Utf8.lexeme buf)
  | "extends" -> EXTENDS
  | "opens" -> OPENS
  | '{' -> BEG_SCOPE
  | '}' -> END_SCOPE
  | "var" -> VAR
  | "print" -> PRINT
  | var_identifier -> ID (Utf8.lexeme buf)
  | ';' -> SEMICOLON
  | any ->
      let pos = buf |> lexing_positions |> fst in
      let _ = next buf in
      raise @@ LexError (pos, "Unknown character: " ^ Utf8.lexeme buf)
  | _ -> assert false

let lex buf = with_tokenizer token buf
