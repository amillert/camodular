%{
  open Env
  open Repr
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

%start <repr> program

%%

let program :=
  | EOF; { Empty }
  | s = scope; { s }

(* class defined as either: empty, non-empty, extended, opened *)
let scope :=
  (* TODO: why EOF ? *)
  | CLASS; classID = ID; EOF; { classNoVars classID }
  | CLASS; classID = ID; vars = variables; { classYesVars classID vars }
  | CLASS; classID = ID; EXTENDS; extendedID = ID; vars = variables;
    (* the extending module exposes inherited variables further *)
    {
      let updatedScope =
        let s = Env.findInEnv extendedID in
        if List.length s.exposed > 0 then Util.dedup s.exposed vars else vars
      in
      ClassScope { id = classID; vars = updatedScope }
    }
  | CLASS; classID = ID; OPENS; openedID = ID; vars = variables;
    (* the opening module doesn't expose inherited variables further *)
    {
      let s = Env.findInEnv openedID in
      (* TODO: ain't the same as exposed ... *)
      let updatedScope =
        if List.length s.exposed > 0 then Util.dedup s.exposed vars else vars
      in
      ClassScope { id = classID; vars = updatedScope }
    }

let variables :=
  | BEG_SCOPE; END_SCOPE; { [] }
  | BEG_SCOPE; VAR; id = ID; SEMICOLON; END_SCOPE; { [id] }
  | BEG_SCOPE; VAR; id = ID; SEMICOLON; otherIDs = variables; END_SCOPE;
    { Util.dedup [id] otherIDs }
