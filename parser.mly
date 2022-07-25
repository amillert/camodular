%{
  open Ast
  open Env

  let update_scope ?(mode = EXTENDS) classID superID vars =
    let open Sem in
    let updatedScope =
      let s = Env.find_in_env superID in
      if List.length s.exposed > 0 then Util.dedup s.exposed vars else vars
    in
    match mode with
    | EXTENDS -> classYesVars classID updatedScope
    | OPENS ->
        let mode = Open superID in
        classYesVars classID updatedScope ~mode:mode (* TODO: remove :mode *)
    | EOF -> Empty
    | _ -> Invalid

  let make_env ast =
    let () = Env.add_to_env ast in
    ast
%}

%token CLASS

%token EXTENDS
%token OPENS

%token BEG_SCOPE
%token END_SCOPE

%token VAR
%token <string> ID (* reused for storing both class and var ids *)
%token SEMICOLON

%token EOF

%start <ast> program

%%

let program :=
  | EOF; { Empty }
  | s = scope; { s }

(* class defined as either: empty, non-empty, extended, opened *)
let scope :=
  (* TODO: why EOF ? *)
  | CLASS; classID = ID; EOF; { make_env @@ classNoVars classID }
  | CLASS; classID = ID; vars = variables; { make_env @@ classYesVars classID vars }
  | CLASS; classID = ID; EXTENDS; extendedID = ID; vars = variables;
    { make_env @@ update_scope classID extendedID vars }
  | CLASS; classID = ID; OPENS; openedID = ID; vars = variables;
    { make_env @@ update_scope classID openedID vars ~mode:OPENS }

let variables :=
  | BEG_SCOPE; END_SCOPE; { [] }
  | BEG_SCOPE; VAR; id = ID; SEMICOLON; END_SCOPE; { [id] }
  | BEG_SCOPE; VAR; id = ID; SEMICOLON; otherIDs = variables; END_SCOPE;
    { Util.dedup [id] otherIDs }
