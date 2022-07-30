%{
  open Repr.Ast

  let make_scope classID vars mode = classYesVars classID vars ~mode

  let make_env ?(prints=[]) ast =
    let open Env in
    let () = Env.add_to_env ~prints ast in
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

%token PRINT

%token EOF

%type <Repr.Ast.ast> scope
%type <string list * string list> variables

%start <Repr.Ast.ast> program

%%

let program :=
  | ~=scope; { scope }
  | EOF; { Empty }

let scope ==
  | CLASS; classID = ID; EOF; { make_env @@ classNoVars classID }
  | CLASS; classID = ID; BEG_SCOPE; END_SCOPE; { make_env @@ classNoVars classID }
  | CLASS; classID = ID; EXTENDS; extendedID = ID;
    { make_env @@ make_scope classID [] (Extend extendedID) }
  | CLASS; classID = ID; OPENS; openedID = ID;
    { make_env @@ make_scope classID [] (Open openedID) }
  | CLASS; classID = ID; BEG_SCOPE; (vars, prints) = variables;
    { let scope = classYesVars classID vars in make_env scope ~prints }
  | CLASS; classID = ID; EXTENDS; extendedID = ID; BEG_SCOPE; (vars, prints) = variables;
    { let scope = make_scope classID vars (Extend extendedID) in make_env scope ~prints }
  | CLASS; classID = ID; OPENS; openedID = ID; BEG_SCOPE; (vars, prints) = variables;
    { let scope = make_scope classID vars (Open openedID) in make_env scope ~prints }

let variables :=
  | END_SCOPE; { [], [] }
  | VAR; id = ID; SEMICOLON; (otherIDs, otherPrints) = variables; END_SCOPE;
    { (Util.dedup [id] otherIDs, otherPrints) }
  | PRINT; id = ID; SEMICOLON; (otherIDs, otherPrints) = variables; END_SCOPE;
    { (otherIDs, Util.dedup [id] otherPrints) }
