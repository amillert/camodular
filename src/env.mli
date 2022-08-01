module Env : sig
  open Repr.Ast
  open Repr.Scope

  type 'a t

  (* env is a running dependency environment
     to disambiguate the class scope. *)
  val env : 'a t ref

  exception EnvException of string

  (* find_in_env searches for a derived class in env. *)
  val find_in_env : string -> scope

  (* add_to_env either injects the class scope
     in env or updates already defined. *)
  val add_to_env : prints:id list -> ast -> unit
end
