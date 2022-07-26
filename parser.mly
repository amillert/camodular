%{
  open Ast

  let make_scope classID vars mode = classYesVars classID vars ~mode

  let make_env ast =
    let open Env in
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

%type <Ast.ast> scope
%type <string list> variables

%start <Ast.ast> program

%%

let program :=
  | EOF; { Empty }
  | s = scope; { s }

let scope :=
  | CLASS; classID = ID; EOF; { make_env @@ classNoVars classID }
  | CLASS; classID = ID; vars = variables; { make_env @@ classYesVars classID vars }
  | CLASS; classID = ID; EXTENDS; extendedID = ID; vars = variables;
    { make_env @@ make_scope classID vars (Extend extendedID) }
  | CLASS; classID = ID; OPENS; openedID = ID; vars = variables;
    { make_env @@ make_scope classID vars (Open openedID) }

let variables :=
  | BEG_SCOPE; END_SCOPE; { [] }
  | BEG_SCOPE; VAR; id = ID; SEMICOLON; END_SCOPE; { [id] }
  | BEG_SCOPE; VAR; id = ID; SEMICOLON; otherIDs = variables; END_SCOPE;
    { Util.dedup [id] otherIDs }
