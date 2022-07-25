open Ast
open Scope

module Env : sig
  type 'a t

  (* env is a running dependency environment
     to disambiguate the class scope. *)
  val env : 'a t ref

  (* find_in_env searches for a derived class in env. *)
  val find_in_env : string -> scope

  (* add_to_env either injects the class scope
     in env or updates already defined. *)
  val add_to_env : ast -> unit
end
